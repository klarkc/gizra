module Web.Element.Form (textareaWysiwygField) where

import Application.Script.Prelude
  ( GetModelName,
    HasField,
    KnownSymbol,
    MetaBag,
    Proxy,
    (<>),
  )
import IHP.MailPrelude
  ( InputType (TextareaInput),
    InputValue,
  )
import IHP.View.Form (textField, textareaField)
import IHP.View.Types
  ( FormContext,
    FormField,
    fieldType,
  )
import Web.View.Prelude (FormField (fieldClass))

-- | Renders a wysiwyg textarea
--
-- >>> {textareaWysiwygField #body}
-- <div class="form-group" id="form-group-post_body">
--     <label for="post_body">Body</label>
--     <textarea name="body" id="post_body" class="form-control" />
-- </div>
--
-- See 'textField' for examples of possible form control options.
textareaWysiwygField ::
  forall fieldName model value.
  ( ?formContext :: FormContext model,
    HasField fieldName model value,
    HasField "meta" model MetaBag,
    KnownSymbol fieldName,
    InputValue value,
    KnownSymbol (GetModelName model)
  ) =>
  Proxy fieldName ->
  FormField
textareaWysiwygField field =
  let f = textareaField field
   in f {fieldClass = f.fieldClass <> "wysiwyg"}
{-# INLINE textareaWysiwygField #-}
