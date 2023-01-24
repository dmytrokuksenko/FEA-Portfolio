'www.PredictiveEngineering.com
'All Rights Reserved, 2018
'Predictive Engineering Assumes No Responsibility For Results Obtained From API
'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'This API was originally coded for FEMAP v12
'Written by Adrian Jensen
'Exports shear and axial force for selected beam elements

Sub Main
	Dim App As femap.model
	Set App = feFemap()

'Create Element Object
	Dim elemOBJ As femap.Elem
	Set elemOBJ = App.feElem

'Dimension an Element Variable
	Dim elemID As Long

'Create Set Object (for Elements)
	Dim elemSET As femap.Set
	Set elemSET = App.feSet

'Dimension an Output Set ID Variable
	Dim outSetID As Long

'Create Set Object (for Output Sets)
	Dim outSetSET As femap.Set
	Set outSetSET = App.feSet

'Create Output Vector Objects
	Dim outVec1 As femap.Output
	Set outVec1 = App.feOutput
	Dim outVec2 As femap.Output
	Set outVec2 = App.feOutput
	Dim outVec3 As femap.Output
	Set outVec3 = App.feOutput
	Dim outVec4 As femap.Output
	Set outVec4 = App.feOutput
	Dim outVec5 As femap.Output
	Set outVec5 = App.feOutput
	Dim outVec6 As femap.Output
	Set outVec6 = App.feOutput

'Dimension an Output Vector Variable
	Dim axial As Double
	Dim pl1shr As Double
	Dim pl2shr As Double
	Dim resshr As Double
	Dim torque As Double
	Dim pl1moment As Double
	Dim pl2moment As Double

'Create Excel Application, Workbook and Worksheet Objects. Be sure to activate the Microsoft Excel 12.0 Obeject Library in References
	Dim appExcel As Excel.Application
	Set appExcel =  New Excel.Application
	Dim wbkReport As Excel.Workbook
	Set wbkReport = appExcel.Workbooks.Add
	Dim wksReport As Excel.Worksheet
	Set wksReport = wbkReport.Worksheets(1)

'Dimension a Variable for use with Excel Objects
	Dim rowIndex As Long
   	rowIndex = 1

'Ask the API to create the titles in the first row of the worksheet
	wksReport.Cells( rowIndex, 1 ) ="Output Set ID"
	wksReport.Cells( rowIndex, 2 ) ="ID"
	wksReport.Cells( rowIndex, 3 ) ="Beam Axial Force"
	wksReport.Cells( rowIndex, 4 ) ="Beam Resultant Shear Force"
	wksReport.Cells( rowIndex, 5 ) ="Beam Pl1 Shear Force"
	wksReport.Cells( rowIndex, 6 ) ="Beam Pl2 Shear Force"
	wksReport.Cells( rowIndex, 7 ) ="Beam Torque"
	wksReport.Cells( rowIndex, 8 ) ="Beam Pl1 Moment"
	wksReport.Cells( rowIndex, 9 ) ="Beam Pl2 Moment"

'Put output sets into ouSetSet and elements into elemSET with an Entity Seclection Dialogue Boxes
	rc = outSetSET.SelectMultiID(FT_OUT_CASE, 1, "Select Output Sets")

	rc = elemSET.Select(FT_ELEM, True, "Select Elements")

'Starting with the lowest numbered output set, cycle through all selected
	outSetID = outSetSET.First
	While outSetID > 0
		outVec1.setID = outSetID
		outVec2.setID = outSetID
		outVec3.setID = outSetID
        outVec4.setID = outSetID
        outVec5.setID = outSetID
        outVec6.setID = outSetID

'Retrieve desired beam output vectors
		rc = outVec1.Get( 3022 ) 'Beam Axial Force
		rc = outVec2.Get( 3018 ) 'Beam Pl1 Shear Force
		rc = outVec3.Get( 3019 ) 'Beam Pl2 Shear Force
		rc = outVec4.Get( 3024 ) 'Beam Torque
		rc = outVec4.Get( 3014 ) 'Beam Pl1 Moment
		rc = outVec4.Get( 3015 ) 'Beam Pl2 Moment

'Starting with the lowest numbered element, cycle through all selected
		elemID = elemSET.First
		While elemID > 0
			elemOBJ.Get (elemID)

'Write output data in sequential Excel rows for beam elements only
			If elemOBJ.type = FET_L_BEAM Then
				rowIndex = rowIndex + 1
				axial = outVec1.value( elemOBJ.ID )
				pl1shr = outVec2.value( elemOBJ.ID )
				pl2shr = outVec3.value( elemOBJ.ID )
				torque = outVec4.value( elemOBJ.ID )
				resshr = ((pl1shr)^2 + (pl2shr)^2)^0.5
  				wksReport.Cells(rowIndex, 1).value = outSetID
  				wksReport.Cells(rowIndex, 2).value = elemOBJ.ID
  				wksReport.Cells(rowIndex, 3).value = axial
  				wksReport.Cells(rowIndex, 4).value = resshr
  				wksReport.Cells(rowIndex, 5).value = pl1shr
  				wksReport.Cells(rowIndex, 6).value = pl2shr
  				wksReport.Cells(rowIndex, 7).value = torque
  				wksReport.Cells(rowIndex, 8).value = pl1moment
  				wksReport.Cells(rowIndex, 9).value = pl2moment
			End If
			elemID = elemSET.Next
		Wend
		outSetID = outSetSET.Next
		rowIndex = rowIndex + 1
	Wend

'Make the Excel spreadsheet visible
	appExcel.Visible = True

End Sub