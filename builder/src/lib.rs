use std::ops::Not;

use proc_macro2::{Ident, Span, TokenStream};
use quote::{IdentFragment, ToTokens, format_ident, quote_spanned};
use syn::{
  Attribute,
  Data,
  DataStruct,
  DeriveInput,
  Error,
  Expr,
  GenericArgument,
  Index,
  ItemFn,
  LitStr,
  Meta,
  PathArguments,
  Type,
  Visibility,
  parse_macro_input,
  parse_quote,
  parse_quote_spanned,
  spanned::Spanned,
};

#[derive(Clone)]
struct StructParsed
{
  errors: Vec<Error>,
  fields: Vec<StructFieldParsed>,
  no_default: bool,
}
#[derive(Clone)]
struct StructFieldParsed
{
  name: Option<Ident>,
  name_or_index: StructFieldNameParsed,
  span: Span,
  ty: StructFieldTypeParsed,
  skip_set: bool,
  skip_get: bool,
  skip: bool,
}
#[derive(Clone)]
enum StructFieldTypeParsed
{
  Normal
  {
    ty: Type
  },
  Option
  {
    inner_ty: Type
  },
  Vec
  {
    inner_ty: Type,
    each: Option<LitStr>,
  },
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
  new: TokenStream,
  init: TokenStream,
  setter: Option<TokenStream>,
  getter: Option<TokenStream>,
  build: TokenStream,
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
  fn from_iter<T: IntoIterator<Item=StructFieldTokens>>(iter: T) -> Self
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

impl StructFieldParsed
{
  pub fn fn_set(&self, vis: &Visibility) -> TokenStream
  {
    let field_index = &self.name_or_index;
    let field_ident = self.name_or_index.to_ident("unknown");
    let param_name = self.name.as_ref().unwrap_or(&field_ident);
    let param_ty = self.ty.ty_fn_set();
    match &self.ty
    {
      StructFieldTypeParsed::Normal { .. } =>
        {
          let fn_name = param_name;
          quote_spanned! {self.span=>
          #vis fn #fn_name(&mut self, #param_name: #param_ty) -> &mut Self {
            self.#field_index = Some(#param_name);
            self
          }
        }
        }
      StructFieldTypeParsed::Option { .. } =>
        {
          let fn_name = param_name;
          quote_spanned! {self.span=>
          #vis fn #fn_name(&mut self, #param_name: #param_ty) -> &mut Self {
            self.#field_index = Some(#param_name);
            self
          }
        }
        }
      StructFieldTypeParsed::Vec { each: Some(each), .. } =>
        {
          let fn_name = Ident::new(&each.value(), self.span);
          let param_name = &fn_name;
          quote_spanned! {self.span=>
          #vis fn #fn_name(&mut self, #param_name: #param_ty) -> &mut Self {
            self.#field_index.extend([#param_name].into_iter());
            self
          }
        }
        }
      StructFieldTypeParsed::Vec { each: _, .. } =>
        {
          let fn_name = param_name;
          quote_spanned! {self.span=>
          #vis fn #fn_name(&mut self, #param_name: #param_ty) -> &mut Self {
            self.#field_index.extend(#param_name);
            self
          }
        }
        }
    }
  }

  pub fn fn_get(&self, vis: &Visibility, is_mut: bool) -> TokenStream
  {
    let field_index = &self.name_or_index;
    let field_ident = self.name_or_index.to_ident("unknown");
    let param_name = self.name.as_ref().unwrap_or(&field_ident);
    let ret_ty = self.ty.ty_get(is_mut);
    let as_fn_name = if is_mut {
      Ident::new("as_mut", Span::call_site())
    } else {
      Ident::new("as_ref", Span::call_site())
    };
    let ref_tokens = if is_mut {
      quote_spanned! {self.span=>
        &mut
      }
    } else {
      quote_spanned! {self.span=>
        &
      }
    };
    let fn_name = if is_mut { Ident::new(&format!("get_mut_{param_name}"), self.span) } else { Ident::new(&format!("get_{param_name}"), self.span) };
    match &self.ty
    {
      StructFieldTypeParsed::Normal { .. } =>
        {
          quote_spanned! {self.span=>
          #vis fn #fn_name(&mut self) -> #ret_ty {
            self.#field_index.#as_fn_name()
          }
        }
        }
      StructFieldTypeParsed::Option { .. } =>
        {
          quote_spanned! {self.span=>
          #vis fn #fn_name(&mut self) -> #ret_ty {
            self.#field_index.#as_fn_name()
          }
        }
        }
      StructFieldTypeParsed::Vec { each: Some(_each), .. } =>
        {
          quote_spanned! {self.span=>
          #vis fn #fn_name(&mut self) -> #ret_ty {
            #ref_tokens self.#field_index
          }
        }
        }
      StructFieldTypeParsed::Vec { each: _, .. } =>
        {
          quote_spanned! {self.span=>
          #vis fn #fn_name(&mut self) -> #ret_ty {
            #ref_tokens self.#field_index
          }
        }
        }
    }
  }
}

impl StructFieldTypeParsed
{
  pub fn ty_get(&self, is_mut: bool) -> Type
  {
    let ref_tokens = if is_mut {
      quote_spanned! {Span::call_site()=>
        &mut
      }
    } else {
      quote_spanned! {Span::call_site()=>
        &
      }
    };
    match self
    {
      StructFieldTypeParsed::Normal { ty } =>
        {
          parse_quote_spanned! {
          ty.span() => ::core::option::Option<#ref_tokens #ty>
        }
        }
      StructFieldTypeParsed::Option { inner_ty } =>
        {
          parse_quote_spanned! {
          inner_ty.span() => ::core::option::Option<#ref_tokens #inner_ty>
        }
        }
      StructFieldTypeParsed::Vec { inner_ty, .. } =>
        {
          parse_quote_spanned! {
          inner_ty.span() => &[#inner_ty]
        }
        }
    }
  }

  pub fn ty_to_builder(&self) -> Type
  {
    match self
    {
      StructFieldTypeParsed::Normal { ty } =>
        {
          parse_quote_spanned! {
            ty.span() => ::core::option::Option<#ty>
          }
        }
      StructFieldTypeParsed::Option { inner_ty } =>
        {
          parse_quote_spanned! {
            inner_ty.span() => ::core::option::Option<#inner_ty>
          }
        }
      StructFieldTypeParsed::Vec { inner_ty, .. } =>
        {
          parse_quote_spanned! {
            inner_ty.span() => ::std::vec::Vec<#inner_ty>
          }
        }
    }
  }

  pub fn ty_fn_set(&self) -> Type
  {
    match self
    {
      StructFieldTypeParsed::Normal { ty } => ty.clone(),
      StructFieldTypeParsed::Option { inner_ty } => inner_ty.clone(),
      StructFieldTypeParsed::Vec { inner_ty, each: Some(_each) } =>
        {
          inner_ty.clone()
        }
      StructFieldTypeParsed::Vec { inner_ty, each: _ } =>
        {
          parse_quote_spanned! {
            inner_ty.span() => ::std::vec::Vec<#inner_ty>
          }
        }
    }
  }

  pub fn tokens_take(&self, expr: &Expr) -> TokenStream
  {
    match self
    {
      StructFieldTypeParsed::Normal { ty } =>
        {
          quote_spanned! {
            ty.span() => ::core::option::Option::<#ty>::take(&mut #expr)?
          }
        }
      StructFieldTypeParsed::Option { inner_ty } =>
        {
          quote_spanned! {
            inner_ty.span() => ::core::option::Option::<#inner_ty>::take(&mut #expr)
          }
        }
      StructFieldTypeParsed::Vec { inner_ty, .. } =>
        {
          quote_spanned! {
            inner_ty.span() => ::core::mem::take(&mut #expr)
          }
        }
    }
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
  } else {
    None
  }
}

trait RefMap<T, E> {
  fn ref_map<U, F>(self, op: F) -> Result<U, E>
  where
    F: FnOnce(&T) -> U;
}

impl<T, E> RefMap<T, E> for Result<T, E> {
  fn ref_map<U, F>(self, op: F) -> Result<U, E>
  where
    F: FnOnce(&T) -> U,
  {
    match self {
      Ok(ok) => Ok(op(&ok)),
      Err(err) => Err(err),
    }
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
    .map(Attribute::parse_args::<Ident>).filter_map(|ident| {
    if let Ok(ident) = &ident && ident == "no_default"
    {
      if no_default
      {
        return Some(Error::new_spanned(ident, "already set 'no_default'"));
      }
      no_default = true;
    }
    ident.err()
  }).collect();
  // .fold(Vec::new(), |acc, result| match (acc, result)
  // {
  //   (errs, Ok(())) => errs,
  //   (mut errs, Err(e)) =>
  //     {
  //       errs.push(e);
  //       errs
  //     }
  // });
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
                      Error::new_spanned(&meta.path, "already set 'skip'")
                    })?;
                    skip = true;
                    Ok(())
                  }
                "skip_set" =>
                  {
                    skip_set.not().ok_or_else(|| {
                      Error::new_spanned(&meta.path, "already set 'skip_set'")
                    })?;
                    skip_set = true;
                    Ok(())
                  }
                "skip_get" =>
                  {
                    skip_get.not().ok_or_else(|| {
                      Error::new_spanned(&meta.path, "already set 'skip_get'")
                    })?;
                    skip_get = true;
                    Ok(())
                  }
                "each" =>
                  {
                    let each_ident: LitStr = meta.value()?.parse()?;
                    if let Some(each) = &each
                    {
                      Err(Error::new_spanned(
                        &meta.path,
                        each.as_ref().map(LitStr::value).map_or_else(
                          |()| "<error>".to_string(),
                          |ident| format!("already set 'each = {ident}'", ),
                        ),
                      ))
                    } else {
                      #[cfg(debug_assertions)]
                      eprintln!("[DEBUG] each = {}", each_ident.to_token_stream());
                      each = Some(Ok(each_ident));
                      Ok(())
                    }
                  }
                other => Err(Error::new_spanned(
                  &meta.path,
                  format!("unknown attribute `{other}`"),
                )),
              }
            } else {
              Err(Error::new_spanned(
                &attr,
                format!("except identifier, but `{}`", attr.to_token_stream()),
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
      let ty = match split_type_to_outer_inner(&field.ty)
      {
        Some((outer_ty, inner_ty)) if outer_ty == "Option" =>
          {
            StructFieldTypeParsed::Option { inner_ty: inner_ty.clone() }
          }
        Some((outer_ty, inner_ty)) if outer_ty == "Vec" =>
          {
            let each = each.and_then(|result| result.ok());
            StructFieldTypeParsed::Vec { inner_ty: inner_ty.clone(), each }
          }
        _ => StructFieldTypeParsed::Normal { ty: field.ty.clone() },
      };
      errors.extend(field_attr_errors);
      StructFieldParsed {
        name,
        name_or_index,
        span,
        ty,
        skip_set,
        skip_get,
        skip,
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
  let field_ty = &field.ty;
  let builder_field_ty: Type = field_ty.ty_to_builder();

  let new = quote_spanned! {span=>
    #name: #builder_field_ty,
  };

  let init = quote_spanned! {span=>
    #name: ::core::option::None,
  };

  let setter = field.skip_set.not().then(|| field.fn_set(&vis));
  let getter = field.skip_get.not().then(|| {
    let mut tokens = field.fn_get(&vis, false);
    tokens.extend([field.fn_get(&vis, true)].into_iter());
    tokens
  });

  let take = field_ty.tokens_take(&parse_quote_spanned! {span=>
    self.#name
  });

  let build = quote_spanned! {span=>
    #name: #take,
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
        } else {
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
