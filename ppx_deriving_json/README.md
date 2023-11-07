# ppx_deriving_json

Generate JSON encoders/decoders for your data types:

```
val to_json : t -> json
val of_json : json -> t
```

works both in native (using `yojson` JSON representation) and melange (using
`Js.Json.t` JSON representation.

