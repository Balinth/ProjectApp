// ts2fable 0.8.0
module rec PrimeReact.DataTable
open System
open Fable.Core
open Fable.React
open Fable.Core.JS
open Browser.Types
open JsInterop

importAll "primereact/resources/themes/nova-dark/theme.css"
importAll "primereact/resources/primereact.min.css"
importAll "primeicons/primeicons.css"


//type IExports = {
//    DataTable of DataTableStatic}

[<RequireQualifiedAccess>]
type DataTableProps =
    | Id  of string option
    | Value of ResizeArray<obj>
    | Header of string list option
    | Footer of obj option
    | Style of obj option
    | ClassName of string option
    | TableStyle of obj option
    | TableClassName of string option
    | Paginator of bool option
    | PaginatorPosition of string option
    | AlwaysShowPaginator of bool option
    | PaginatorTemplate of string option
    | PaginatorLeft of obj option
    | PaginatorRight of obj option
    | PageLinkSize of float option
    | RowsPerPageOptions of ResizeArray<float> option
    | CurrentPageReportTemplate of string option
    | First of float option
    | Rows of float option
    | TotalRecords of float option
    | Lazy of bool option
    | SortField of string option
    | SortOrder of float option
    | MultiSortMeta of ResizeArray<obj option> option
    | SortMode of string option
    | DefaultSortOrder of float option
    | RemovableSort of bool option
    | EmptyMessage of string option
    | SelectionMode of string option
    | Selection of obj option
    | ContextMenuSelection of obj option
    | CompareSelectionBy of string option
    | DataKey of string option
    | MetaKeySelection of bool option
    | HeaderColumnGroup of obj option
    | FooterColumnGroup of obj option
    | FrozenHeaderColumnGroup of obj option
    | FrozenFooterColumnGroup of obj option
    | ExpandedRows of ResizeArray<obj option> option
    | Responsive of bool option
    | ResizableColumns of bool option
    | ColumnResizeMode of string option
    | ReorderableColumns of bool option
    | Filters of obj option
    | GlobalFilter of obj option
    | Scrollable of bool option
    | ScrollHeight of string option
    | VirtualScroll of bool option
    | VirtualScrollDelay of float option
    | VirtualRowHeight of float option
    | FrozenWidth of string option
    | FrozenValue of ResizeArray<obj option> option
    | CsvSeparator of string option
    | ExportFilename of string option
    | RowGroupMode of string option
    | AutoLayout of bool option
    | Loading of bool option
    | LoadingIcon of string option
    | TabIndex of string option
    | StateKey of string option
    | StateStorage of string option
    | GroupField of string option
    | EditMode of string option
    | ExpandableRowGroups of bool option
    | OnSelectionChange of e : (DataTablePropsOnSelectionChangeE -> unit)
    | OnContextMenuSelectionChange of e : (DataTablePropsOnContextMenuSelectionChangeE -> unit)
    | RowExpansionTemplate of data : (obj option -> Element option)
    | OnRowToggle of e : ( DataTablePropsOnRowToggleE -> unit)
    | RowClassName of rowData : ( obj option -> obj)
    | RowGroupHeaderTemplate of data : ( obj option * float -> ReactElement option)
    | RowGroupFooterTemplate of data : ( obj option * float -> ReactElement option)
    | OnColumnResizeEnd of e : ( DataTablePropsOnColumnResizeEndE -> unit)
    | OnSort of e : ( DataTablePropsOnSortE -> unit)
    | OnPage of e : ( DataTablePropsOnPageE -> unit)
    | OnFilter of filters : ( ResizeArray<obj option> -> unit)
    | OnVirtualScroll of e : ( DataTablePropsOnVirtualScrollE -> unit)
    | OnRowClick of e : ( DataTablePropsOnRowClickE -> unit)
    | OnRowDoubleClick of e : ( DataTablePropsOnRowDoubleClickE -> unit)
    | OnRowSelect of e : ( DataTablePropsOnRowSelectE -> unit)
    | OnRowUnselect of e : ( DataTablePropsOnRowUnselectE -> unit)
    | OnRowExpand of e : ( DataTablePropsOnRowExpandE -> unit)
    | OnRowCollapse of e : ( DataTablePropsOnRowCollapseE -> unit)
    | OnContextMenu of e : ( DataTablePropsOnContextMenuE -> unit)
    | OnColReorder of e : ( DataTablePropsOnColReorderE -> unit)
    | OnRowReorder of e : ( DataTablePropsOnRowReorderE -> unit)
    | OnValueChange of value : ( ResizeArray<obj option> -> unit)
    | RowEditorValidator of rowData : ( obj option -> bool)
    | OnRowEditInit of e : ( DataTablePropsOnRowEditInitE -> unit)
    | OnRowEditSave of e : ( DataTablePropsOnRowEditSaveE -> unit)
    | OnRowEditCancel of e : ( DataTablePropsOnRowEditCancelE -> unit)
    | ExportFunction of e : ( DataTablePropsExportFunctionE -> obj option)

type DataTablePropsOnSelectionChangeE ={
    originalEvent : Event
    value : obj option
}

type DataTablePropsOnContextMenuSelectionChangeE = {
    originalEvent : Event
    value : obj option
}

type DataTablePropsOnRowToggleE = {
    data : ResizeArray<obj option>}

type DataTablePropsOnColumnResizeEndE = {
    element : HTMLElement
    delta : float}

type DataTablePropsOnSortE = {
    sortField : string
    sortOrder : float
    multiSortMeta : obj option}

type DataTablePropsOnPageE = {
    first : float
    rows : float}

type DataTablePropsOnVirtualScrollE = {
    first : float
    rows : float}

type DataTablePropsOnRowClickE = {
    originalEvent : Event
    data : obj option
    index : float}

type DataTablePropsOnRowDoubleClickE = {
    originalEvent : Event
    data : obj option
    index : float}

type DataTablePropsOnRowSelectE = {
    originalEvent : Event
    data : obj option
    ``type`` : string}

type DataTablePropsOnRowUnselectE = {
    originalEvent : Event
    data : obj option
    ``type`` : string}

type DataTablePropsOnRowExpandE = {
    originalEvent : Event
    data : obj option}

type DataTablePropsOnRowCollapseE = {
    originalEvent : Event
    data : obj option}

type DataTablePropsOnContextMenuE = {
    originalEvent : Event
    data : obj option}

type DataTablePropsOnColReorderE = {
    originalEvent : Event
    dragIndex : float
    dropIndex : float
    columns : obj option}

type DataTablePropsOnRowReorderE = {
    originalEvent : Event
    value : obj option
    dragIndex : float
    dropIndex : float}

type DataTablePropsOnRowEditInitE = {
    originalEvent : Event
    data : obj option}

type DataTablePropsOnRowEditSaveE = {
    originalEvent : Event
    data : obj option}

type DataTablePropsOnRowEditCancelE = {
    originalEvent : Event
    data : obj option
    index : float}

type DataTablePropsExportFunctionE = {
    data : obj option
    field : string}

type DataTable = {
    reset : unit -> unit
    exportCSV : unit -> unit
    filter : obj * string * string -> unit
    resetColumnOrder : unit -> unit
    closeEditingCell : unit -> unit
}
    //inherit Component<DataTableProps, obj option>

//[<Import("*", "../../node-modules/primereact")>]
//let DataTableLib : DataTable = jsNative
//let primreactStuff : obj = importAll "primereact/datatable"
//let dataTable (props : DataTableProps) : DataTable = import "Datatable" "primereact/datatable"

//[<ImportAll("../../node_modules/primereact/datatable.js")>]
//let DataTableLib : ReactElement = jsNative


let inline DataTableBuilder (props : DataTableProps list) (children : seq<ReactElement>) : ReactElement =
    ofImport "DataTable" @"primereact/datatable" (keyValueList CaseRules.LowerFirst props) children
