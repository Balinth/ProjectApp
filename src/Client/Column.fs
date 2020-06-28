module rec PrimeReact.Column
open System
open Fable.Core
open Fable.React
open Fable.Core.JS
open Browser.Types
open JsInterop

importAll "primereact/resources/themes/nova-dark/theme.css"
importAll "primereact/resources/primereact.min.css"
importAll "primeicons/primeicons.css"
//type [<AllowNullLiteral>] IExports =
//    abstract Column: ColumnStatic
[<RequireQualifiedAccess>]
type ColProps =
    | ColumnKey of string 
    | Field of string 
    | SortField of string 
    | Header of string 
    | Body of obj 
    | LoadingBody of obj 
    | Footer of obj 
    | Sortable of bool 
    | Filter of bool 
    | FilterMatchMode of string 
    | FilterPlaceholder of string 
    | FilterType of string 
    | FilterMaxLength of float 
    | FilterElement of obj 
    | Style of obj 
    | ClassName of string 
    | HeaderStyle of obj 
    | HeaderClassName of string 
    | BodyStyle of obj 
    | BodyClassName of string 
    | FooterStyle of obj 
    | FooterClassName of string 
    | Expander of bool 
    | Frozen of bool 
    | SelectionMode of string 
    | ColSpan of float 
    | RowSpan of float 
    | RowReorder of bool 
    | RowReorderIcon of string 
    | EditorValidatorEvent of string 
    | RowEditor of bool 
    | Exportable of bool 
    | OnEditorSubmit of props: (obj -> unit)
    | OnEditorCancel of props: (obj -> unit)
    | ExcludeGlobalFilter of bool 
    | SortFunction of e: (ColumnPropsSortFunctionE -> unit)
    | FilterFunction of value: obj * filter: (obj -> unit)
    | Editor of props: (obj -> Element) 
    | EditorValidator of props: (obj -> bool)

type ColumnPropsSortFunctionE =
    abstract field: string with get, set
    abstract order: float with get, set

let inline ColBuilder (props : ColProps list) : ReactElement =
    ofImport "Column" @"primereact/column" (keyValueList CaseRules.LowerFirst props) []

//    inherit React.Component<ColumnProps, obj option>
//
//type [<AllowNullLiteral>] ColumnStatic =
//    [<Emit "new $0($1...)">] abstract Create: unit -> Column