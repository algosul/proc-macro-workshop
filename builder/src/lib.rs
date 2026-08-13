use std::ops::Not;

use proc_macro2::{Ident, Span, TokenStream};
use quote::{IdentFragment, ToTokens, format_ident, quote_spanned};
use syn::{
  Attribute,
  Data,
  DataStruct,
  DeriveInput,
  Error,
  GenericArgument,
  Index,
  ItemFn,
  LitStr,
  Meta,
  PathArguments,
  Type,
  Visibility,
  ext::IdentExt,
  parse_macro_input,
  parse_quote,
  spanned::Spanned,
};

#[derive(Clone)]
struct StructParsed
{
  errors:     Vec<Error>,
  fields:     Vec<StructFieldParsed>,
  no_default: bool,
}
#[derive(Clone)]
struct StructFieldParsed
{
  name:          Option<Ident>,
  name_or_index: StructFieldNameParsed,
  span:          Span,
  ty:            Type,
  skip_set:      bool,
  skip_get:      bool,
  skip:          bool,
  each:          Option<LitStr>,
}
#[derive(Clone)]
enum StructFieldNameParsed
{
  Named(Ident),
  Unnamed(usize),
}

#[derive(Default, Debug, Clone)]
struct StructFieldTokens
{
  errors: Vec<Error>,
  new:    TokenStream,
  init:   TokenStream,
  setter: Option<TokenStream>,
  getter: Option<TokenStream>,
  build:  TokenStream,
}

impl StructFieldNameParsed
{
  fn to_ident(&self, unnamed_prefix: impl IdentFragment) -> Ident
  {
    match self
    {
      StructFieldNameParsed::Named(ident) => ident.clone(),
      StructFieldNameParsed::Unnamed(index) =>
      {
        format_ident!("{}{}", unnamed_prefix, Index::from(*index))
      }
    }
  }

  #[allow(unused)]
  fn into_ident(self, unnamed_prefix: impl IdentFragment) -> Ident
  {
    match self
    {
      StructFieldNameParsed::Named(ident) => ident,
      StructFieldNameParsed::Unnamed(index) =>
      {
        format_ident!("{}{}", unnamed_prefix, Index::from(index))
      }
    }
  }
}

impl ToTokens for StructFieldNameParsed
{
  fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream)
  {
    match self
    {
      StructFieldNameParsed::Named(ident) => ident.to_tokens(tokens),
      StructFieldNameParsed::Unnamed(index) =>
      {
        Index::from(*index).to_tokens(tokens)
      }
    }
  }
}

impl FromIterator<StructFieldTokens> for StructFieldTokens
{
  fn from_iter<T: IntoIterator<Item = StructFieldTokens>>(iter: T) -> Self
  {
    iter.into_iter().fold(StructFieldTokens::default(), |mut acc, item| {
      acc.errors.extend(item.errors);
      acc.new.extend(item.new);
      acc.init.extend(item.init);
      match (&mut acc.setter, item.setter)
      {
        (Some(a), Some(b)) =>
        {
          a.extend(b);
        }
        (None, Some(a)) => acc.setter = Some(a),
        (_, None) =>
        {}
      }
      match (&mut acc.getter, item.getter)
      {
        (Some(a), Some(b)) =>
        {
          a.extend(b);
        }
        (None, Some(a)) => acc.getter = Some(a),
        (_, None) =>
        {}
      }
      acc.build.extend(item.build);
      acc
    })
  }
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream
{
  let input = parse_macro_input!(input as DeriveInput);

  do_expend(&input).into()
}

fn split_type_to_outer_inner(ty: &Type) -> Option<(&Ident, &Type)>
{
  if let Type::Path(path) = ty
    && let Some(last) = path.path.segments.last()
    && let PathArguments::AngleBracketed(args) = &last.arguments
    && let GenericArgument::Type(inner_ty) = args.args.first()?
  {
    Some((&last.ident, inner_ty))
  }
  else
  {
    None
  }
}

fn parse_struct(
  struct_attrs: &Vec<Attribute>, data_struct: &DataStruct,
) -> StructParsed
{
  let mut errors = Vec::new();
  let mut no_default = false;

  // parse struct attrs
  let struct_attr_errors: Vec<_> = struct_attrs
    .iter()
    .filter(|attr| attr.meta.path().is_ident("builder"))
    .map(|attr| {
      let ident = attr.parse_args::<Ident>()?;
      if ident.unraw() == "no_default"
      {
        if no_default
        {
          return Err(Error::new_spanned(ident, "already set 'no_default'"));
        }
        no_default = true;
      }
      Ok(())
    })
    .fold(Vec::new(), |acc, result| match (acc, result)
    {
      (errs, Ok(())) => errs,
      (mut errs, Err(e)) =>
      {
        errs.push(e);
        errs
      }
    });
  errors.extend(struct_attr_errors);

  // parse fields
  let fields = data_struct
    .fields
    .iter()
    .enumerate()
    .map(|(i, field)| {
      let name = field.ident.clone();
      let name_or_index = field
        .ident
        .clone()
        .map(StructFieldNameParsed::Named)
        .unwrap_or(StructFieldNameParsed::Unnamed(i));
      let span = field.span();
      let ty = field.ty.clone();
      let mut skip_set = false;
      let mut skip_get = false;
      let mut skip = false;
      let mut each: Option<Result<_, ()>> = None;

      let field_attr_errors: Vec<_> = field
        .attrs
        .iter()
        .filter(|attr| attr.meta.path().is_ident("builder"))
        .map(|attr| {
          attr.parse_nested_meta(|meta| {
            if let Some(ident) = meta.path.get_ident()
            {
              let arg = ident.to_string();
              match arg.as_str()
              {
                "skip" =>
                {
                  skip.not().ok_or_else(|| {
                    Error::new_spanned(arg, "already set 'skip'")
                  })?;
                  skip = true;
                  Ok(())
                }
                "skip_set" =>
                {
                  skip_set.not().ok_or_else(|| {
                    Error::new_spanned(arg, "already set 'skip_set'")
                  })?;
                  skip_set = true;
                  Ok(())
                }
                "skip_get" =>
                {
                  skip_get.not().ok_or_else(|| {
                    Error::new_spanned(arg, "already set 'skip_get'")
                  })?;
                  skip_get = true;
                  Ok(())
                }
                "each" =>
                {
                  let each_ident = meta.value()?.parse::<LitStr>()?;
                  if let Some(each) = &each
                  {
                    Err(Error::new_spanned(
                      &attr,
                      each.as_ref().map(LitStr::value).map_or_else(
                        |()| "<error>".to_string(),
                        |ident| format!("already set 'each = {ident}'",),
                      ),
                    ))
                  }
                  else
                  {
                    each = Some(Ok(each_ident));
                    Ok(())
                  }
                }
                other => Err(Error::new_spanned(
                  &arg,
                  format!("unknown attribute '{other}'"),
                )),
              }
            }
            else
            {
              Err(Error::new_spanned(
                &attr,
                format!("except identifier, but {}", attr.to_token_stream()),
              ))
            }
          })
        })
        .fold(Vec::new(), |acc, result| match (acc, result)
        {
          (errs, Ok(())) => errs,
          (mut errs, Err(e)) =>
          {
            errs.push(e);
            errs
          }
        });
      errors.extend(field_attr_errors);
      let each = each.and_then(Result::ok);
      StructFieldParsed {
        name,
        name_or_index,
        span,
        ty,
        skip_set,
        skip_get,
        skip,
        each,
      }
    })
    .collect();
  StructParsed { errors, fields, no_default }
}

fn generate_field_tokens(
  vis: &Visibility, field: &StructFieldParsed,
) -> StructFieldTokens
{
  let mut errors = Vec::new();
  let unknown_ident =
    field.name_or_index.to_ident(Ident::new("unknown", Span::call_site()));
  let span = field.span;
  let name = field.name.as_ref().unwrap_or_else(|| {
    errors.push(Error::new(
      span.clone(),
      "not set ident, please use #[builder(ident = \"new_ident\")]",
    ));
    &unknown_ident
  });
  let old_field_ty = &field.ty;
  let field_ty_ref: Type;
  let field_ty_mut: Type;
  let new_field_ty: Type;
  let in_vec_ty: Type;
  let param_ty: Type;
  let is_option: bool;
  match split_type_to_outer_inner(&old_field_ty)
  {
    Some((outer, old_field_inner_ty)) if outer == "Option" =>
    {
      is_option = true;
      field_ty_ref = parse_quote!(::core::option::Option<&#old_field_inner_ty>);
      field_ty_mut =
        parse_quote!(::core::option::Option<&mut #old_field_inner_ty>);
      new_field_ty = parse_quote!(::core::option::Option<#old_field_inner_ty>);
      param_ty = old_field_inner_ty.clone();
      in_vec_ty = parse_quote!(!);
    }
    Some((outer, old_field_inner_ty)) if outer == "Vec" =>
    {
      is_option = false;
      field_ty_ref = parse_quote!(::core::option::Option<&#old_field_ty>);
      field_ty_mut = parse_quote!(::core::option::Option<&mut #old_field_ty>);
      new_field_ty = parse_quote!(::std::vec::Vec<#old_field_ty>);
      param_ty = old_field_ty.clone();
      in_vec_ty = old_field_inner_ty.clone();
    }
    _ =>
    {
      is_option = false;
      field_ty_ref = parse_quote!(::core::option::Option<&#old_field_ty>);
      field_ty_mut = parse_quote!(::core::option::Option<&mut #old_field_ty>);
      new_field_ty = parse_quote!(::core::option::Option<#old_field_ty>);
      param_ty = old_field_ty.clone();
      in_vec_ty = parse_quote!(!);
    }
  }

  let new = quote_spanned! {span=>
    #name: #new_field_ty,
  };

  let init = quote_spanned! {span=>
    #name: ::core::option::None,
  };

  let setter = field.skip_set.not().then_some(field.each.as_ref().map_or_else(
    || {
      let fn_name = name;
      quote_spanned! {span=>
        #vis fn #fn_name(&mut self, #name: #param_ty) -> &mut Self {
          self.#name = Some(#name);
          self
        }
      }
    },
    |each| {
      let fn_name = format_ident!("{}", each.value());
      quote_spanned! {span=>
        #vis fn #fn_name(&mut self, one: #in_vec_ty) -> &mut Self {
          self.#name.extend_one(one);
          self
        }
      }
    },
  ));
  let getter = field.skip_get.not().then(|| {
    let fn_name = format_ident!("get_{}", name);
    let mut_fn_name = format_ident!("get_mut_{}", name);
    quote_spanned! {span=>
      #vis fn #fn_name(&self) -> #field_ty_ref {
        self.#name.as_ref()
      }
      #vis fn #mut_fn_name(&mut self) -> #field_ty_mut {
        self.#name.as_mut()
      }
    }
  });

  let build = if is_option
  {
    quote_spanned! {span=>
      #name: self.#name.take(),
    }
  }
  else
  {
    quote_spanned! {span=>
      #name: self.#name.take()?,
    }
  };

  StructFieldTokens { errors, new, init, setter, getter, build }
}

fn do_expend(input: &DeriveInput) -> TokenStream
{
  match &input.data
  {
    Data::Struct(data_struct) =>
    {
      let parsed = parse_struct(&input.attrs, data_struct);

      let ty = &input.ident;
      let vis: Visibility = parse_quote!(pub);
      let builder_ident = format_ident!("{}Builder", ty);

      let StructFieldTokens { mut errors, new, init, setter, getter, build } =
        parsed
          .fields
          .iter()
          .filter(|field| !field.skip)
          .map(|field| generate_field_tokens(&vis, field))
          .collect();

      let (derive_meta, builder_fn): (Meta, ItemFn) = if parsed.no_default
      {
        (
          parse_quote!(derive(Clone, Debug)),
          parse_quote!(
            #vis fn builder() -> #builder_ident {
              #builder_ident { #init }
            }
          ),
        )
      }
      else
      {
        (
          parse_quote!(derive(Clone, Debug, Default)),
          parse_quote!(
            #vis fn builder() -> #builder_ident {
              <#builder_ident as ::core::default::Default>::default()
            }
          ),
        )
      };

      errors.extend(parsed.errors);
      let errors = errors.into_iter().map(Error::into_compile_error);
      let span = input.span();
      quote_spanned! {span=>
        #(#errors)*
        impl #ty {
          #builder_fn
        }
        #[#derive_meta]
        #vis struct #builder_ident {
          #new
        }
        impl #builder_ident {
          #setter
          #getter
          #vis fn build(&mut self) -> ::core::option::Option<#ty> {
            Some(#ty { #build })
          }
        }
      }
      .into()
    }
    Data::Enum(_) => todo!(),
    Data::Union(_) => todo!(),
  }
}
