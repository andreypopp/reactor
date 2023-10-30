module FormData = struct
  external ofFormElement : Dom.htmlFormElement -> Fetch.formData
    = "FormData"
  [@@mel.new]
end
