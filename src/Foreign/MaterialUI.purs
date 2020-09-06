module Foreign.MaterialUI where

import Custom.Prelude

import Concur as C
import MaterialUI.Button (classButton)
import MaterialUI.FormControl (classFormControl)
import MaterialUI.Grid (classGrid)
import MaterialUI.InputLabel (classInputLabel)
import MaterialUI.Link (classLink)
import MaterialUI.MenuItem (classMenuItem)
import MaterialUI.Paper (classPaper)
import MaterialUI.Select (classSelect)
import MaterialUI.Styles (muiThemeProviderClass)
import MaterialUI.Table (classTable)
import MaterialUI.TableBody (classTableBody)
import MaterialUI.TableCell (classTableCell)
import MaterialUI.TableHead (classTableHead)
import MaterialUI.TablePagination (classTablePagination)
import MaterialUI.TableRow (classTableRow)
import MaterialUI.TableSortLabel (classTableSortLabel)
import MaterialUI.TextField (classTextField)
import MaterialUI.Theme (Theme)
import MaterialUI.Tooltip (classTooltip)
import MaterialUI.Typography (classTypography)
import React (ReactClass, unsafeCreateElement)
import React.DOM.Props (unsafeFromPropsArray)

type El = C.El
type El1 = forall a. Array (C.ReactProps a) -> C.Widget C.HTML a -> C.Widget C.HTML a
type El1' = forall a. C.Widget C.HTML a -> C.Widget C.HTML a

------------------------------------------------------------------------------
-- From purescript-react-mui

button :: C.El
button = C.mkEl classButton

formControl :: C.El
formControl = C.mkEl classFormControl

grid :: C.El
grid = C.mkEl classGrid

inputLabel :: C.El
inputLabel = C.mkEl classInputLabel

link :: C.El
link = C.mkEl classLink

menuItem :: C.El
menuItem = C.mkEl classMenuItem

paper :: C.El
paper = C.mkEl classPaper

paper1 :: El1
paper1 props child = paper props [child]

select :: C.El
select = C.mkEl classSelect

select1 :: El1
select1 props child = select props [child]

table :: C.El
table = C.mkEl classTable

tbody :: C.El
tbody = C.mkEl classTableBody

tcell :: C.El
tcell = C.mkEl classTableCell

tcell1 :: El1
tcell1 props child = tcell props [child]

tcell1' :: El1'
tcell1' child = tcell [] [child]

tooltip :: El
tooltip = C.mkEl classTooltip

tooltip1 :: El1
tooltip1 props child = tooltip props [child]

typography :: C.El
typography = C.mkEl classTypography

typography1 :: El1
typography1 props child = typography props [child]

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
foreign import boxImpl :: forall a. ReactClass a
foreign import buttonGroupImpl :: forall a. ReactClass a
foreign import dateFnsUtilsImpl :: Foreign
foreign import pickerProviderImpl :: forall a. ReactClass a
foreign import timePickerImpl :: forall a. ReactClass a
foreign import tableContainerImpl :: forall a. ReactClass a


box :: C.El
box = C.mkEl boxImpl

buttonGroup :: C.El
buttonGroup = C.mkEl buttonGroupImpl

tcontainer :: C.El
tcontainer = C.mkEl tableContainerImpl

timePicker :: C.El
timePicker = C.mkEl timePickerImpl

pickerProvider' :: C.El
pickerProvider' = C.mkEl pickerProviderImpl

pickerProvider :: forall a. C.Widget C.HTML a -> C.Widget C.HTML a
pickerProvider child = pickerProvider' [ C.unsafeMkProp "utils" dateFnsUtilsImpl ] [ child ]

themeProvider :: forall a. Theme -> C.Widget C.HTML a -> C.Widget C.HTML a
themeProvider theme child = C.el'
  (unsafeCreateElement muiThemeProviderClass <<< unsafeFromPropsArray)
  [ C.unsafeMkProp "theme" theme ]
  [ child ]
