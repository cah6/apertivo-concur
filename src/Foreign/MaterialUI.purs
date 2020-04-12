module Foreign.MaterialUI where

import Custom.Prelude

import Concur as C
import MaterialUI.Button (classButton)
import MaterialUI.Grid (classGrid)
import MaterialUI.Paper (classPaper)
import MaterialUI.Table (classTable)
import MaterialUI.TableBody (classTableBody)
import MaterialUI.TableCell (classTableCell)
import MaterialUI.TableHead (classTableHead)
import MaterialUI.TablePagination (classTablePagination)
import MaterialUI.TableRow (classTableRow)
import MaterialUI.TableSortLabel (classTableSortLabel)
import MaterialUI.TextField (classTextField)
import React (ReactClass, unsafeCreateElement)
import React.DOM.Props (unsafeFromPropsArray)


------------------------------------------------------------------------------
-- From purescript-react-mui

foreign import classBox :: forall a. ReactClass a
foreign import classTableContainer :: forall a. ReactClass a

box :: C.El
box = C.mkEl classBox

button :: C.El
button = C.mkEl classButton

grid :: C.El
grid = C.mkEl classGrid

paper :: C.El
paper = C.mkEl classPaper

table :: C.El
table = C.mkEl classTable

tcontainer :: C.El
tcontainer = C.mkEl classTableContainer

tbody :: C.El
tbody = C.mkEl classTableBody

tcell :: C.El
tcell = C.mkEl classTableCell

thead :: C.El
thead = C.mkEl classTableHead

tpagination :: C.El
tpagination = C.mkEl classTablePagination

trow :: C.El
trow = C.mkEl classTableRow

tsort :: C.El
tsort = C.mkEl classTableSortLabel

textField :: C.El
textField = C.mkEl classTextField

------------------------------------------------------------------------------
-- From our own FFI
foreign import timePickerImpl :: forall a. ReactClass a

timePicker :: C.El
timePicker = C.el' (unsafeCreateElement timePickerImpl <<< unsafeFromPropsArray)


foreign import pickerProviderImpl :: forall a. ReactClass a
foreign import dateFnsUtilsImpl :: Foreign

pickerProvider' :: C.El
pickerProvider' = C.el' (unsafeCreateElement pickerProviderImpl <<< unsafeFromPropsArray)


pickerProvider child = pickerProvider' [ C.unsafeMkProp "utils" dateFnsUtilsImpl ] [ child ]


foreign import createMuiTheme :: forall r. {|r} -> Foreign
