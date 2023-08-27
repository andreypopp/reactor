type props = prop list
and prop = string * value

and value =
  [ `String of string | `Bool of bool | `Int of int | `Float of float ]

let s v : value = `String v
let b v : value = `Bool v
let i v : value = `Int v
let f v : value = `Float v
let ariaActivedescendant v = "aria-activedescendant", s v
let ariaAtomic : bool -> prop = fun v -> "aria-atomic", b v
let ariaAutocomplete : string -> prop = fun v -> "aria-autocomplete", s v
let ariaBusy : bool -> prop = fun v -> "aria-busy", b v
let ariaChecked : string -> prop = fun v -> "aria-checked", s v
let ariaColcount : int -> prop = fun v -> "aria-colcount", i v
let ariaColindex : int -> prop = fun v -> "aria-colindex", i v
let ariaColspan : int -> prop = fun v -> "aria-colspan", i v
let ariaControls : string -> prop = fun v -> "aria-controls", s v
let ariaCurrent : string -> prop = fun v -> "aria-current", s v
let ariaDescribedby : string -> prop = fun v -> "aria-describedby", s v
let ariaDetails : string -> prop = fun v -> "aria-details", s v
let ariaDisabled : bool -> prop = fun v -> "aria-disabled", b v
let ariaErrormessage : string -> prop = fun v -> "aria-errormessage", s v
let ariaExpanded : bool -> prop = fun v -> "aria-expanded", b v
let ariaFlowto : string -> prop = fun v -> "aria-flowto", s v
let ariaGrabbed : bool -> prop = fun v -> "aria-grabbed", b v
let ariaHaspopup : string -> prop = fun v -> "aria-haspopup", s v
let ariaHidden : bool -> prop = fun v -> "aria-hidden", b v
let ariaInvalid : string -> prop = fun v -> "aria-invalid", s v
let ariaKeyshortcuts : string -> prop = fun v -> "aria-keyshortcuts", s v
let ariaLabel : string -> prop = fun v -> "aria-label", s v
let ariaLabelledby : string -> prop = fun v -> "aria-labelledby", s v
let ariaLevel : int -> prop = fun v -> "aria-level", i v
let ariaLive : string -> prop = fun v -> "aria-live", s v
let ariaModal : bool -> prop = fun v -> "aria-modal", b v
let ariaMultiline : bool -> prop = fun v -> "aria-multiline", b v

let ariaMultiselectable : bool -> prop =
 fun v -> "aria-multiselectable", b v

let ariaOrientation : string -> prop = fun v -> "aria-orientation", s v
let ariaOwns : string -> prop = fun v -> "aria-owns", s v
let ariaPlaceholder : string -> prop = fun v -> "aria-placeholder", s v
let ariaPosinset : int -> prop = fun v -> "aria-posinset", i v
let ariaPressed : string -> prop = fun v -> "aria-pressed", s v
let ariaReadonly : bool -> prop = fun v -> "aria-readonly", b v
let ariaRelevant : string -> prop = fun v -> "aria-relevant", s v
let ariaRequired : bool -> prop = fun v -> "aria-required", b v

let ariaRoledescription : string -> prop =
 fun v -> "aria-roledescription", s v

let ariaRowcount : int -> prop = fun v -> "aria-rowcount", i v
let ariaRowindex : int -> prop = fun v -> "aria-rowindex", i v
let ariaRowindextext : string -> prop = fun v -> "aria-rowindextext", s v
let ariaRowspan : int -> prop = fun v -> "aria-rowspan", i v
let ariaSelected : bool -> prop = fun v -> "aria-selected", b v
let ariaSetsize : int -> prop = fun v -> "aria-setsize", i v
let ariaSort : string -> prop = fun v -> "aria-sort", s v
let ariaValuemax : float -> prop = fun v -> "aria-valuemax", f v
let ariaValuemin : float -> prop = fun v -> "aria-valuemin", f v
let ariaValuenow : float -> prop = fun v -> "aria-valuenow", f v
let ariaValuetext : string -> prop = fun v -> "aria-valuetext", s v
let defaultChecked : bool -> prop = fun v -> "defaultChecked", b v
let defaultValue : string -> prop = fun v -> "defaultValue", s v
let accessKey : string -> prop = fun v -> "accessKey", s v
let className : string -> prop = fun v -> "className", s v
let contentEditable : bool -> prop = fun v -> "contentEditable", b v
let contextMenu : string -> prop = fun v -> "contextMenu", s v
let dir : string -> prop = fun v -> "dir", s v
let draggable : bool -> prop = fun v -> "draggable", b v
let hidden : bool -> prop = fun v -> "hidden", b v
let id : string -> prop = fun v -> "id", s v
let lang : string -> prop = fun v -> "lang", s v
let role : string -> prop = fun v -> "role", s v

(* let style : style -> prop = fun v -> "style", s v *)
let spellCheck : bool -> prop = fun v -> "spellCheck", b v
let tabIndex : int -> prop = fun v -> "tabIndex", i v
let title : string -> prop = fun v -> "title", s v
let itemID : string -> prop = fun v -> "itemID", s v
let itemProp : string -> prop = fun v -> "itemProp", s v
let itemRef : string -> prop = fun v -> "itemRef", s v
let itemScope : bool -> prop = fun v -> "itemScope", b v
let itemType : string -> prop = fun v -> "itemType", s v
let as_ : string -> prop = fun v -> "as", s v
let accept : string -> prop = fun v -> "accept", s v
let acceptCharset : string -> prop = fun v -> "acceptCharset", s v
let action : string -> prop = fun v -> "action", s v
let allowFullScreen : bool -> prop = fun v -> "allowFullScreen", b v
let alt : string -> prop = fun v -> "alt", s v
let async : bool -> prop = fun v -> "async", b v
let autoComplete : string -> prop = fun v -> "autoComplete", s v
let autoCapitalize : string -> prop = fun v -> "autoCapitalize", s v
let autoFocus : bool -> prop = fun v -> "autoFocus", b v
let autoPlay : bool -> prop = fun v -> "autoPlay", b v
let challenge : string -> prop = fun v -> "challenge", s v
let charSet : string -> prop = fun v -> "charSet", s v
let checked : bool -> prop = fun v -> "checked", b v
let cite : string -> prop = fun v -> "cite", s v
let crossorigin : bool -> prop = fun v -> "crossorigin", b v
let cols : int -> prop = fun v -> "cols", i v
let colSpan : int -> prop = fun v -> "colSpan", i v
let content : string -> prop = fun v -> "content", s v
let controls : bool -> prop = fun v -> "controls", b v
let coords : string -> prop = fun v -> "coords", s v
let data : string -> prop = fun v -> "data", s v
let dateTime : string -> prop = fun v -> "dateTime", s v
let default : bool -> prop = fun v -> "default", b v
let defer : bool -> prop = fun v -> "defer", b v
let disabled : bool -> prop = fun v -> "disabled", b v
let download : string -> prop = fun v -> "download", s v
let encType : string -> prop = fun v -> "encType", s v
let form : string -> prop = fun v -> "form", s v
let formAction : string -> prop = fun v -> "formAction", s v
let formTarget : string -> prop = fun v -> "formTarget", s v
let formMethod : string -> prop = fun v -> "formMethod", s v
let headers : string -> prop = fun v -> "headers", s v
let height : string -> prop = fun v -> "height", s v
let high : int -> prop = fun v -> "high", i v
let href : string -> prop = fun v -> "href", s v
let hrefLang : string -> prop = fun v -> "hrefLang", s v
let htmlFor : string -> prop = fun v -> "htmlFor", s v
let httpEquiv : string -> prop = fun v -> "httpEquiv", s v
let icon : string -> prop = fun v -> "icon", s v
let inputMode : string -> prop = fun v -> "inputMode", s v
let integrity : string -> prop = fun v -> "integrity", s v
let keyType : string -> prop = fun v -> "keyType", s v
let kind : string -> prop = fun v -> "kind", s v
let label : string -> prop = fun v -> "label", s v
let list : string -> prop = fun v -> "list", s v
let loop : bool -> prop = fun v -> "loop", b v
let low : int -> prop = fun v -> "low", i v
let manifest : string -> prop = fun v -> "manifest", s v
let max : string -> prop = fun v -> "max", s v
let maxLength : int -> prop = fun v -> "maxLength", i v
let media : string -> prop = fun v -> "media", s v
let mediaGroup : string -> prop = fun v -> "mediaGroup", s v
let _method : string -> prop = fun v -> "_method", s v
let min : string -> prop = fun v -> "min", s v
let minLength : int -> prop = fun v -> "minLength", i v
let multiple : bool -> prop = fun v -> "multiple", b v
let muted : bool -> prop = fun v -> "muted", b v
let name : string -> prop = fun v -> "name", s v
let nonce : string -> prop = fun v -> "nonce", s v
let noValidate : bool -> prop = fun v -> "noValidate", b v
let open_ : bool -> prop = fun v -> "open", b v
let optimum : int -> prop = fun v -> "optimum", i v
let pattern : string -> prop = fun v -> "pattern", s v
let placeholder : string -> prop = fun v -> "placeholder", s v
let poster : string -> prop = fun v -> "poster", s v
let preload : string -> prop = fun v -> "preload", s v
let radioGroup : string -> prop = fun v -> "radioGroup", s v
let readOnly : bool -> prop = fun v -> "readOnly", b v
let rel : string -> prop = fun v -> "rel", s v
let required : bool -> prop = fun v -> "required", b v
let reversed : bool -> prop = fun v -> "reversed", b v
let rows : int -> prop = fun v -> "rows", i v
let rowSpan : int -> prop = fun v -> "rowSpan", i v
let sandbox : string -> prop = fun v -> "sandbox", s v
let scope : string -> prop = fun v -> "scope", s v
let scoped : bool -> prop = fun v -> "scoped", b v
let scrolling : string -> prop = fun v -> "scrolling", s v
let selected : bool -> prop = fun v -> "selected", b v
let shape : string -> prop = fun v -> "shape", s v
let size : int -> prop = fun v -> "size", i v
let sizes : string -> prop = fun v -> "sizes", s v
let span : int -> prop = fun v -> "span", i v
let src : string -> prop = fun v -> "src", s v
let srcDoc : string -> prop = fun v -> "srcDoc", s v
let srcLang : string -> prop = fun v -> "srcLang", s v
let srcSet : string -> prop = fun v -> "srcSet", s v
let start : int -> prop = fun v -> "start", i v
let step : float -> prop = fun v -> "step", f v
let summary : string -> prop = fun v -> "summary", s v
let target : string -> prop = fun v -> "target", s v
let type_ : string -> prop = fun v -> "type", s v
let useMap : string -> prop = fun v -> "useMap", s v
let value : string -> prop = fun v -> "value", s v
let width : string -> prop = fun v -> "width", s v
let wrap : string -> prop = fun v -> "wrap", s v
let accentHeight : string -> prop = fun v -> "accentHeight", s v
let accumulate : string -> prop = fun v -> "accumulate", s v
let additive : string -> prop = fun v -> "additive", s v
let alignmentBaseline : string -> prop = fun v -> "alignmentBaseline", s v
let allowReorder : string -> prop = fun v -> "allowReorder", s v
let alphabetic : string -> prop = fun v -> "alphabetic", s v
let amplitude : string -> prop = fun v -> "amplitude", s v
let arabicForm : string -> prop = fun v -> "arabicForm", s v
let ascent : string -> prop = fun v -> "ascent", s v
let attributeName : string -> prop = fun v -> "attributeName", s v
let attributeType : string -> prop = fun v -> "attributeType", s v
let autoReverse : string -> prop = fun v -> "autoReverse", s v
let azimuth : string -> prop = fun v -> "azimuth", s v
let baseFrequency : string -> prop = fun v -> "baseFrequency", s v
let baseProfile : string -> prop = fun v -> "baseProfile", s v
let baselineShift : string -> prop = fun v -> "baselineShift", s v
let bbox : string -> prop = fun v -> "bbox", s v
let begin_ : string -> prop = fun v -> "begin", s v
let bias : string -> prop = fun v -> "bias", s v
let by : string -> prop = fun v -> "by", s v
let calcMode : string -> prop = fun v -> "calcMode", s v
let capHeight : string -> prop = fun v -> "capHeight", s v
let clip : string -> prop = fun v -> "clip", s v
let clipPath : string -> prop = fun v -> "clipPath", s v
let clipPathUnits : string -> prop = fun v -> "clipPathUnits", s v
let clipRule : string -> prop = fun v -> "clipRule", s v

let colorInterpolation : string -> prop =
 fun v -> "colorInterpolation", s v

let colorInterpolationFilters : string -> prop =
 fun v -> "colorInterpolationFilters", s v

let colorProfile : string -> prop = fun v -> "colorProfile", s v
let colorRendering : string -> prop = fun v -> "colorRendering", s v
let contentScriptType : string -> prop = fun v -> "contentScriptType", s v
let contentStyleType : string -> prop = fun v -> "contentStyleType", s v
let cursor : string -> prop = fun v -> "cursor", s v
let cx : string -> prop = fun v -> "cx", s v
let cy : string -> prop = fun v -> "cy", s v
let d : string -> prop = fun v -> "d :", s v
let decelerate : string -> prop = fun v -> "decelerate", s v
let descent : string -> prop = fun v -> "descent", s v
let diffuseConstant : string -> prop = fun v -> "diffuseConstant", s v
let direction : string -> prop = fun v -> "direction", s v
let display : string -> prop = fun v -> "display", s v
let divisor : string -> prop = fun v -> "divisor", s v
let dominantBaseline : string -> prop = fun v -> "dominantBaseline", s v
let dur : string -> prop = fun v -> "dur", s v
let dx : string -> prop = fun v -> "dx", s v
let dy : string -> prop = fun v -> "dy", s v
let edgeMode : string -> prop = fun v -> "edgeMode", s v
let elevation : string -> prop = fun v -> "elevation", s v
let enableBackground : string -> prop = fun v -> "enableBackground", s v
let end_ : string -> prop = fun v -> "end", s v
let exponent : string -> prop = fun v -> "exponent", s v

let externalResourcesRequired : string -> prop =
 fun v -> "externalResourcesRequired", s v

let fill : string -> prop = fun v -> "fill", s v
let fillOpacity : string -> prop = fun v -> "fillOpacity", s v
let fillRule : string -> prop = fun v -> "fillRule", s v
let filter : string -> prop = fun v -> "filter", s v
let filterRes : string -> prop = fun v -> "filterRes", s v
let filterUnits : string -> prop = fun v -> "filterUnits", s v
let floodColor : string -> prop = fun v -> "floodColor", s v
let floodOpacity : string -> prop = fun v -> "floodOpacity", s v
let focusable : string -> prop = fun v -> "focusable", s v
let fontFamily : string -> prop = fun v -> "fontFamily", s v
let fontSize : string -> prop = fun v -> "fontSize", s v
let fontSizeAdjust : string -> prop = fun v -> "fontSizeAdjust", s v
let fontStretch : string -> prop = fun v -> "fontStretch", s v
let fontStyle : string -> prop = fun v -> "fontStyle", s v
let fontVariant : string -> prop = fun v -> "fontVariant", s v
let fontWeight : string -> prop = fun v -> "fontWeight", s v
let fomat : string -> prop = fun v -> "fomat", s v
let from : string -> prop = fun v -> "from", s v
let fx : string -> prop = fun v -> "fx", s v
let fy : string -> prop = fun v -> "fy", s v
let g1 : string -> prop = fun v -> "g1", s v
let g2 : string -> prop = fun v -> "g2", s v
let glyphName : string -> prop = fun v -> "glyphName", s v

let glyphOrientationHorizontal : string -> prop =
 fun v -> "glyphOrientationHorizontal", s v

let glyphOrientationVertical : string -> prop =
 fun v -> "glyphOrientationVertical", s v

let glyphRef : string -> prop = fun v -> "glyphRef", s v
let gradientTransform : string -> prop = fun v -> "gradientTransform", s v
let gradientUnits : string -> prop = fun v -> "gradientUnits", s v
let hanging : string -> prop = fun v -> "hanging", s v
let horizAdvX : string -> prop = fun v -> "horizAdvX", s v
let horizOriginX : string -> prop = fun v -> "horizOriginX", s v
let ideographic : string -> prop = fun v -> "ideographic", s v
let imageRendering : string -> prop = fun v -> "imageRendering", s v
let in_ : string -> prop = fun v -> "in", s v
let in2 : string -> prop = fun v -> "in2", s v
let intercept : string -> prop = fun v -> "intercept", s v
let k : string -> prop = fun v -> "k :", s v
let k1 : string -> prop = fun v -> "k1", s v
let k2 : string -> prop = fun v -> "k2", s v
let k3 : string -> prop = fun v -> "k3", s v
let k4 : string -> prop = fun v -> "k4", s v
let kernelMatrix : string -> prop = fun v -> "kernelMatrix", s v
let kernelUnitLength : string -> prop = fun v -> "kernelUnitLength", s v
let kerning : string -> prop = fun v -> "kerning", s v
let keyPoints : string -> prop = fun v -> "keyPoints", s v
let keySplines : string -> prop = fun v -> "keySplines", s v
let keyTimes : string -> prop = fun v -> "keyTimes", s v
let lengthAdjust : string -> prop = fun v -> "lengthAdjust", s v
let letterSpacing : string -> prop = fun v -> "letterSpacing", s v
let lightingColor : string -> prop = fun v -> "lightingColor", s v
let limitingConeAngle : string -> prop = fun v -> "limitingConeAngle", s v
let local : string -> prop = fun v -> "local", s v
let markerEnd : string -> prop = fun v -> "markerEnd", s v
let markerHeight : string -> prop = fun v -> "markerHeight", s v
let markerMid : string -> prop = fun v -> "markerMid", s v
let markerStart : string -> prop = fun v -> "markerStart", s v
let markerUnits : string -> prop = fun v -> "markerUnits", s v
let markerWidth : string -> prop = fun v -> "markerWidth", s v
let mask : string -> prop = fun v -> "mask", s v
let maskContentUnits : string -> prop = fun v -> "maskContentUnits", s v
let maskUnits : string -> prop = fun v -> "maskUnits", s v
let mathematical : string -> prop = fun v -> "mathematical", s v
let mode : string -> prop = fun v -> "mode", s v
let numOctaves : string -> prop = fun v -> "numOctaves", s v
let offset : string -> prop = fun v -> "offset", s v
let opacity : string -> prop = fun v -> "opacity", s v
let operator : string -> prop = fun v -> "operator", s v
let order : string -> prop = fun v -> "order", s v
let orient : string -> prop = fun v -> "orient", s v
let orientation : string -> prop = fun v -> "orientation", s v
let origin : string -> prop = fun v -> "origin", s v
let overflow : string -> prop = fun v -> "overflow", s v
let overflowX : string -> prop = fun v -> "overflowX", s v
let overflowY : string -> prop = fun v -> "overflowY", s v
let overlinePosition : string -> prop = fun v -> "overlinePosition", s v
let overlineThickness : string -> prop = fun v -> "overlineThickness", s v
let paintOrder : string -> prop = fun v -> "paintOrder", s v
let panose1 : string -> prop = fun v -> "panose1", s v
let pathLength : string -> prop = fun v -> "pathLength", s v

let patternContentUnits : string -> prop =
 fun v -> "patternContentUnits", s v

let patternTransform : string -> prop = fun v -> "patternTransform", s v
let patternUnits : string -> prop = fun v -> "patternUnits", s v
let pointerEvents : string -> prop = fun v -> "pointerEvents", s v
let points : string -> prop = fun v -> "points", s v
let pointsAtX : string -> prop = fun v -> "pointsAtX", s v
let pointsAtY : string -> prop = fun v -> "pointsAtY", s v
let pointsAtZ : string -> prop = fun v -> "pointsAtZ", s v
let preserveAlpha : string -> prop = fun v -> "preserveAlpha", s v

let preserveAspectRatio : string -> prop =
 fun v -> "preserveAspectRatio", s v

let primitiveUnits : string -> prop = fun v -> "primitiveUnits", s v
let r : string -> prop = fun v -> "r :", s v
let radius : string -> prop = fun v -> "radius", s v
let refX : string -> prop = fun v -> "refX", s v
let refY : string -> prop = fun v -> "refY", s v
let renderingIntent : string -> prop = fun v -> "renderingIntent", s v
let repeatCount : string -> prop = fun v -> "repeatCount", s v
let repeatDur : string -> prop = fun v -> "repeatDur", s v

let requiredExtensions : string -> prop =
 fun v -> "requiredExtensions", s v

let requiredFeatures : string -> prop = fun v -> "requiredFeatures", s v
let restart : string -> prop = fun v -> "restart", s v
let result : string -> prop = fun v -> "result", s v
let rotate : string -> prop = fun v -> "rotate", s v
let rx : string -> prop = fun v -> "rx", s v
let ry : string -> prop = fun v -> "ry", s v
let scale : string -> prop = fun v -> "scale", s v
let seed : string -> prop = fun v -> "seed", s v
let shapeRendering : string -> prop = fun v -> "shapeRendering", s v
let slope : string -> prop = fun v -> "slope", s v
let spacing : string -> prop = fun v -> "spacing", s v
let specularConstant : string -> prop = fun v -> "specularConstant", s v
let specularExponent : string -> prop = fun v -> "specularExponent", s v
let speed : string -> prop = fun v -> "speed", s v
let spreadMethod : string -> prop = fun v -> "spreadMethod", s v
let startOffset : string -> prop = fun v -> "startOffset", s v
let stdDeviation : string -> prop = fun v -> "stdDeviation", s v
let stemh : string -> prop = fun v -> "stemh", s v
let stemv : string -> prop = fun v -> "stemv", s v
let stitchTiles : string -> prop = fun v -> "stitchTiles", s v
let stopColor : string -> prop = fun v -> "stopColor", s v
let stopOpacity : string -> prop = fun v -> "stopOpacity", s v

let strikethroughPosition : string -> prop =
 fun v -> "strikethroughPosition", s v

let strikethroughThickness : string -> prop =
 fun v -> "strikethroughThickness", s v

let string : string -> prop = fun v -> "string", s v
let stroke : string -> prop = fun v -> "stroke", s v
let strokeDasharray : string -> prop = fun v -> "strokeDasharray", s v
let strokeDashoffset : string -> prop = fun v -> "strokeDashoffset", s v
let strokeLinecap : string -> prop = fun v -> "strokeLinecap", s v
let strokeLinejoin : string -> prop = fun v -> "strokeLinejoin", s v
let strokeMiterlimit : string -> prop = fun v -> "strokeMiterlimit", s v
let strokeOpacity : string -> prop = fun v -> "strokeOpacity", s v
let strokeWidth : string -> prop = fun v -> "strokeWidth", s v
let surfaceScale : string -> prop = fun v -> "surfaceScale", s v
let systemLanguage : string -> prop = fun v -> "systemLanguage", s v
let tableValues : string -> prop = fun v -> "tableValues", s v
let targetX : string -> prop = fun v -> "targetX", s v
let targetY : string -> prop = fun v -> "targetY", s v
let textAnchor : string -> prop = fun v -> "textAnchor", s v
let textDecoration : string -> prop = fun v -> "textDecoration", s v
let textLength : string -> prop = fun v -> "textLength", s v
let textRendering : string -> prop = fun v -> "textRendering", s v
let to_ : string -> prop = fun v -> "to", s v
let transform : string -> prop = fun v -> "transform", s v
let u1 : string -> prop = fun v -> "u1", s v
let u2 : string -> prop = fun v -> "u2", s v
let underlinePosition : string -> prop = fun v -> "underlinePosition", s v

let underlineThickness : string -> prop =
 fun v -> "underlineThickness", s v

let unicode : string -> prop = fun v -> "unicode", s v
let unicodeBidi : string -> prop = fun v -> "unicodeBidi", s v
let unicodeRange : string -> prop = fun v -> "unicodeRange", s v
let unitsPerEm : string -> prop = fun v -> "unitsPerEm", s v
let vAlphabetic : string -> prop = fun v -> "vAlphabetic", s v
let vHanging : string -> prop = fun v -> "vHanging", s v
let vIdeographic : string -> prop = fun v -> "vIdeographic", s v
let vMathematical : string -> prop = fun v -> "vMathematical", s v
let values : string -> prop = fun v -> "values", s v
let vectorEffect : string -> prop = fun v -> "vectorEffect", s v
let version : string -> prop = fun v -> "version", s v
let vertAdvX : string -> prop = fun v -> "vertAdvX", s v
let vertAdvY : string -> prop = fun v -> "vertAdvY", s v
let vertOriginX : string -> prop = fun v -> "vertOriginX", s v
let vertOriginY : string -> prop = fun v -> "vertOriginY", s v
let viewBox : string -> prop = fun v -> "viewBox", s v
let viewTarget : string -> prop = fun v -> "viewTarget", s v
let visibility : string -> prop = fun v -> "visibility", s v
let widths : string -> prop = fun v -> "widths", s v
let wordSpacing : string -> prop = fun v -> "wordSpacing", s v
let writingMode : string -> prop = fun v -> "writingMode", s v
let x : string -> prop = fun v -> "x :", s v
let x1 : string -> prop = fun v -> "x1", s v
let x2 : string -> prop = fun v -> "x2", s v
let xChannelSelector : string -> prop = fun v -> "xChannelSelector", s v
let xHeight : string -> prop = fun v -> "xHeight", s v
let xlinkActuate : string -> prop = fun v -> "xlinkActuate", s v
let xlinkArcrole : string -> prop = fun v -> "xlinkArcrole", s v
let xlinkHref : string -> prop = fun v -> "xlinkHref", s v
let xlinkRole : string -> prop = fun v -> "xlinkRole", s v
let xlinkShow : string -> prop = fun v -> "xlinkShow", s v
let xlinkTitle : string -> prop = fun v -> "xlinkTitle", s v
let xlinkType : string -> prop = fun v -> "xlinkType", s v
let xmlns : string -> prop = fun v -> "xmlns", s v
let xmlnsXlink : string -> prop = fun v -> "xmlnsXlink", s v
let xmlBase : string -> prop = fun v -> "xmlBase", s v
let xmlLang : string -> prop = fun v -> "xmlLang", s v
let xmlSpace : string -> prop = fun v -> "xmlSpace", s v
let y : string -> prop = fun v -> "y :", s v
let y1 : string -> prop = fun v -> "y1", s v
let y2 : string -> prop = fun v -> "y2", s v
let yChannelSelector : string -> prop = fun v -> "yChannelSelector", s v
let z : string -> prop = fun v -> "z :", s v
let zoomAndPan : string -> prop = fun v -> "zoomAndPan", s v
let about : string -> prop = fun v -> "about", s v
let datatype : string -> prop = fun v -> "datatype", s v
let inlist : string -> prop = fun v -> "inlist", s v
let prefix : string -> prop = fun v -> "prefix", s v
let property : string -> prop = fun v -> "property", s v
let resource : string -> prop = fun v -> "resource", s v
let typeof : string -> prop = fun v -> "typeof", s v
let vocab : string -> prop = fun v -> "vocab", s v
(* let suppressHydrationWarning : bool option option; [@mel.optional] *)
(* let suppressContentEditableWarning : bool option; [@mel.optional] *)