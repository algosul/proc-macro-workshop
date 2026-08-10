use std::ops::Not;

use proc_macro2::{Ident, Span, TokenStream};
use quote::{format_ident, quote_spanned, IdentFragment, ToTokens};
use syn::{
  ext::IdentExt,
  parse_macro_input,
  parse_quote,
  spanned::Spanned,
  Attribute,
  Data,
  DataStruct,
  DeriveInput,
  Error,
  ExprAssign,
  GenericArgument,
  Index,
  ItemFn,
  Meta,
  PathArguments,
  Type,
  Visibility,
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
  each:          Option<Ident>,
}
#[derive(Clone)]
enum StructFieldNameParsed
{
  Named(Ident),
  Unnamed(usize),
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

#[proc_macro_derive(Builder)]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream
{
  let input = parse_macro_input!(input as DeriveInput);

  do_expend(&input).into()
}

fn get_inner_type(ty: &Type) -> Option<Type>
{
  get_inner_type_if(ty, |_| true)
}

fn get_inner_type_if(ty: &Type, f: impl FnOnce(&Ident) -> bool)
-> Option<Type>
{
  if let Type::Path(path) = ty
  {
    path.path.segments.last().and_then(|last| {
      f(&last.ident).then(|| None::<Type>)?;
      if let PathArguments::AngleBracketed(args) = &last.arguments
      {
        if let Some(GenericArgument::Type(inner_ty)) = args.args.first()
        {
          Some(inner_ty.clone())
        }
        else
        {
          None
        }
      }
      else
      {
        None
      }
    })
  }
  else
  {
    None
  }
}

fn get_option_inner_type(ty: &Type) -> Option<Type>
{
  get_inner_type_if(ty, |ident| ident == "Option")
}

fn get_vec_inner_type(ty: &Type) -> Option<Type>
{
  get_inner_type_if(ty, |ident| ident == "Vec")
}

fn parse_struct(
  struct_attrs: &Vec<Attribute>, data_struct: &DataStruct,
) -> StructParsed
{
  let mut errors = Vec::new();
  let mut no_default = false;
  let struct_attr: Result<(), Vec<_>> = struct_attrs
    .iter()
    .filter(|attr| attr.meta.path().is_ident("builder"))
    .map(|attr| {
      attr
        .parse_args::<Ident>()
        .map(|ident| {
          if ident.unraw() == "no_default"
          {
            if no_default
            {
              return Err(Error::new_spanned(
                ident,
                "already set 'no_default'",
              ));
            }
            no_default = true;
          }
          Ok(())
        })
        .and_then(|inner| inner)
    })
    .fold(Ok(()), |acc, result| match (acc, result)
    {
      (Ok(()), Ok(())) => Ok(()),
      (Ok(()), Err(e)) => Err(vec![e]),
      (Err(errs), Ok(())) => Err(errs),
      (Err(mut errs), Err(e)) =>
      {
        errs.push(e);
        Err(errs)
      }
    });
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
      let skip_set = false;
      let skip_get = false;
      let mut skip = false;
      let mut each = None;
      for attr in field.attrs.iter()
      {
        if !attr.meta.path().is_ident("builder")
        {
          continue;
        }
        match attr.parse_args::<Ident>()
        {
          Ok(arg) =>
          {
            let arg = arg.unraw();
            if arg == "skip"
            {
              if skip
              {
                errors.push(Error::new_spanned(arg, "already set 'skip'"));
              }
              skip = true;
            }
          }
          Err(err) =>
          {
            errors.push(err);
          }
        };
        match attr.parse_args::<ExprAssign>()
        {
          Ok(expr) =>
          {
            todo!();
            if let Some(each) = each
            {
              errors.push(Error::new_spanned(
                expr,
                format!("already set 'each = {each}'"),
              ));
            }
            each = Some(todo!());
          }
          Err(err) =>
          {
            errors.push(err);
          }
        };
      }
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
  StructParsed { errors: Vec::new(), fields, no_default }
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

      let (new_fields, init_fields, fields_setter_getter, build_fields): (
        TokenStream,
        TokenStream,
        TokenStream,
        TokenStream,
      ) = parsed
        .fields
        .iter()
        .filter(|field| !field.skip)
        .map(|field| {
          let mut unknown_ident_error = None;
          let unknown_ident = field
            .name_or_index
            .to_ident(Ident::new("unknown", Span::call_site()));
          let span = field.span;
          let name = field.name.as_ref().unwrap_or_else(|| {
            unknown_ident_error = Some(Error::new(
              span.clone(),
              "not set ident, please use #[builder(ident = \"new_ident\")]",
            ));
            &unknown_ident
          });
          let old_field_ty = &field.ty;
          let field_ty_ref: Type;
          let field_ty_mut: Type;
          let option_inner_ty: Type;
          let inner_ty: Type;
          let param_ty: &Type;
          let is_option: bool;
          match get_option_inner_type(&old_field_ty)
          {
            Some(old_field_inner_ty) =>
            {
              is_option = true;
              inner_ty = old_field_inner_ty;
            }
            None =>
            {
              is_option = false;
              inner_ty = old_field_ty.clone();
            }
          }
          field_ty_ref = parse_quote!(::core::option::Option<&#inner_ty>);
          field_ty_mut = parse_quote!(::core::option::Option<&mut #inner_ty>);
          option_inner_ty = parse_quote!(::core::option::Option<#inner_ty>);
          param_ty = &inner_ty;

          let setter = field.skip_set.not().then(|| {
            let fn_name = name;
            quote_spanned! {span=>
              #vis fn #fn_name(&mut self, #name: #param_ty) -> &mut Self {
                self.#name = Some(#name);
                self
              }
            }
          });
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

          let build_field = if is_option
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

          let unknown_ident_error =
            unknown_ident_error.map(Error::into_compile_error);
          (
            quote_spanned! {span=>
              #name: #option_inner_ty,
            },
            quote_spanned! {span=>
              #name: ::core::option::None,
            },
            quote_spanned! {span=>
              #unknown_ident_error
              #setter
              #getter
            },
            build_field,
          )
        })
        .collect();

      let (derive_meta, builder_fn): (Meta, ItemFn) = if parsed.no_default
      {
        (
          parse_quote!(derive(Clone, Debug)),
          parse_quote!(
            #vis fn builder() -> #builder_ident {
              #builder_ident { #init_fields }
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

      let errors = parsed.errors.into_iter().map(Error::into_compile_error);
      let span = input.span();
      quote_spanned! {span=>
        #(#errors)*
        impl #ty {
          #builder_fn
        }
        #[#derive_meta]
        #vis struct #builder_ident {
          #new_fields
        }
        impl #builder_ident {
          #fields_setter_getter
          #vis fn build(&mut self) -> ::core::option::Option<#ty> {
            Some(#ty { #build_fields })
          }
        }
      }
      .into()
    }
    Data::Enum(_) => todo!(),
    Data::Union(_) => todo!(),
  }
}
