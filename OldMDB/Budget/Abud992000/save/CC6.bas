Attribute VB_Name = "Module1"
Option Explicit
Public FirstRow, FirstCol As Integer
Public LastRow, LastCol As Integer
Public FirstRowMvt, FirstColMvt As Integer
Public LastRowMvt, LastColMvt As Integer
Public CurRow, CurCol, CurRowinOut As Integer
Public EndofRapport As Boolean
Public WildCard As String
'Public a, wrk As String
'Public b As Integer
'Public v As Variant
Public StartOfLibelle, EndOfLibelle As Integer
Sub Clear_and_Paste_In()
'
' Clear_and_Paste_In Macro
' Macro recorded 17/02/00 by J-C Bar
'
'
    Call DeleteSheetIn
    Call CreateSheetIN
    Range("A1").Select
    ActiveSheet.PasteSpecial Format:="Text", Link:=False, DisplayAsIcon:= _
        False
End Sub
Function BackInstring(startpos As Integer, wrkstr As String _
, s As String) As Integer
Dim i, j, imax As Integer
Dim w, ww As String
Dim Found As Boolean
imax = Len(wrkstr)
Found = False
i = 1
ww = Left(wrkstr, imax - startpos + 1)
While i <= Len(ww) And Not Found
    w = Right(ww, i)
    j = InStr(1, w, s)
    If j > 0 Then Found = True
    i = i + 1
    Wend
BackInstring = Len(ww) - i + j + 1
End Function
Sub CopyDateMvtCCNew(inrow As Integer, incol As Integer, outrow As Integer, outcol As Integer)
Dim w As String
Dim a, b As Integer
w = Sheets!in.Cells(inrow, incol).Value
a = InStr(1, w, " ")
w = Mid(w, 1, a - 1)
w = Mid(w, 4, 2) & "/" & Left(w, 2) & "/" & Right(w, 4)
Sheets!out.Cells(outrow, outcol).Value = w
Sheets!out.Cells(outrow, outcol).NumberFormat = "dd-mmm-yy"
End Sub
Sub CopyDateValCCNew(inrow As Integer, incol As Integer, outrow As Integer, outcol As Integer)
Dim w As String
Dim a, b As Integer
w = Sheets!in.Cells(inrow, incol).Value
a = InStr(1, w, ": ")
w = Mid(w, a + 2, 10)
w = Mid(w, 4, 2) & "/" & Left(w, 2) & "/" & Right(w, 4)
Sheets!out.Cells(outrow, outcol).Value = w
Sheets!out.Cells(outrow, outcol).NumberFormat = "dd-mmm-yy"
End Sub
Sub CopyMontantCCNew(inrow As Integer, incol As Integer, outrow As Integer, outcol As Integer)
Dim a, b, c, signe As Integer
Dim w, ww As String
w = Sheets!in.Cells(inrow, incol).Value
a = BackInstring(1, (w), "BEF")
b = BackInstring(Len(w) - a + 2, (w), " ") '***
c = BackInstring(Len(w) - b + 2, (w), " ")
w = Mid(Sheets!in.Cells(inrow, incol).Value, c + 1, b - c - 1)
ww = Sheets!in.Cells(inrow, incol).Value
a = InStr(1, ww, "+")
b = InStr(1, ww, "-")
If b > a Then a = 0
If a > 0 Then
        signe = 1
    Else
        signe = -1
    End If
a = InStr(1, w, ".")
    If a > 0 Then
        w = Left(w, a - 1) & Right(w, Len(w) - a)
        a = InStr(1, w, ".")
        If a > 0 Then w = Left(w, a - 1) & Right(w, Len(w) - a)
        End If
Sheets!out.Cells(outrow, outcol).Value = signe * w
Sheets!out.Cells(outrow, outcol).NumberFormat = "#,##0"
End Sub
Sub CopyNumeroCCNew(inrow As Integer, incol As Integer, outrow As Integer, outcol As Integer)
Dim a, b As Integer
Dim w As String
w = Sheets!in.Cells(inrow, incol).Value
a = InStr(1, w, " ")
b = InStr(a + 1, w, " ")
w = Mid(Sheets!in.Cells(inrow, incol).Value, a + 1, b - a - 1)
w = "2001." & w & ".00"
Sheets!out.Cells(outrow, outcol).Value = w
End Sub

Sub CopyLibelleCCNew(inrow As Integer, incol As Integer, _
outrow As Integer, outcol As Integer) ', fr As Integer, _
'lr As Integer, sl As Integer, el As Integer)
Dim w, ww As String
Dim i, a, b As Integer
w = Sheets!in.Cells(inrow, incol).Value
a = InStr(1, w, " ")
i = BackInstring(1, (w), "-")
b = BackInstring(1, (w), "+")
If i > b Then
    b = BackInstring(1, (w), "-")
    Else
      b = BackInstring(1, (w), "+")
    End If
  w = Mid(w, a + 4 + 1, b - a - 5 - 1) '***
i = inrow + 1
While i < LastRowMvt
w = w & "//" & Sheets!in.Cells(i, incol).Value
i = i + 1
Wend
Sheets!out.Cells(outrow, outcol).Value = w
End Sub
Sub auto_open()
UserForm1.Show
End Sub
Sub DeleteSheetIn()
Sheets!in.Delete
End Sub
Sub DeleteSheetOut()
Sheets!out.Delete
End Sub
Sub CreateSheetIN()
Sheets.Add
    ActiveSheet.Name = "In"
    Sheets!in.Activate
End Sub
Sub CreateOutPutCCNew()
Dim fr As Integer
WildCard = " "
Sheets!out.Activate
EndofRapport = False
fr = 1
While EndofRapport = False
If Len(Cells(fr, 1).Value) = 0 Then
EndofRapport = True
Else
fr = fr + 1
End If
Wend
Sheets!in.Activate
Cells.Select
Selection.Replace What:=" ", Replacement:=WildCard, LookAt:=xlPart, _
        SearchOrder:=xlByRows, MatchCase:=False
CurRowinOut = fr
EndofRapport = False
Call InitializeFormat
'Call FindLineDernières
FirstRow = 1
FirstCol = 1
Call findFirstMvtNew((FirstRow), (FirstCol))
CurRow = FirstRow + 1
CurCol = FirstCol
Call FindLastLineofInputCCNew
StartOfLibelle = 10
While EndofRapport = False
    Call findLastLineMvtCCNew((CurRow), (CurCol))
    Call CopyDateMvtCCNew((CurRow), (CurCol), (CurRowinOut), 1)
    Call CopyNumeroCCNew((CurRow), (CurCol), (CurRowinOut), 2)
    Call CopyDateValCCNew((LastRowMvt), (CurCol), (CurRowinOut), 3)
    Call CopyMontantCCNew((CurRow), (CurCol), (CurRowinOut), 4)
    Call CopyLibelleCCNew((CurRow), (CurCol), (CurRowinOut), 5)
    CurRow = LastRowMvt + 2
    If CurRow >= LastRow Then EndofRapport = True
    CurRowinOut = CurRowinOut + 1
Wend
'Call DeleteColumn
Sheets!out.Activate
Cells.Select
Selection.Columns.AutoFit
Cells(1, 1).Select
'UserForm1.Hide
End Sub

Sub InitializeFormat()
Attribute InitializeFormat.VB_Description = "Macro recorded 2/12/99 by J-C Bar"
Attribute InitializeFormat.VB_ProcData.VB_Invoke_Func = " \n14"
'
'
'
Sheets!in.Activate
    Columns("A:A").Select
    Selection.ClearFormats
    Selection.Columns.AutoFit
    Selection.NumberFormat = "@"
    With Selection
        .HorizontalAlignment = xlLeft
        .VerticalAlignment = xlBottom
        .WrapText = False
        .Orientation = 0
        .ShrinkToFit = False
        .MergeCells = False
    End With
'    Sheets!in.Activate
'    Columns("A:A").Select
'    Selection.ClearFormats
'    Selection.Columns.AutoFit
End Sub

Sub FindLastLineofInputCCNew()
'ok
'recherche de la dernière ligne utile de l'input
'
Dim EndofRpt As Boolean
Dim c, rr, a, b As Integer
Sheets!in.Activate
    Columns("A:A").Select
    EndofRpt = False
     rr = FirstRow
    c = 1
    While EndofRpt = False
        rr = rr + 1
        Cells(rr, c).Select
        a = Cells(rr, c).Value
        b = Len(a)
        If b > 0 Then
                EndofRpt = False
            Else
                EndofRpt = True
            End If
        Wend
    LastRow = ActiveCell.Row - 1
    LastCol = ActiveCell.Column
End Sub
Sub findFirstMvtNew(r As Integer, c As Integer)
'   Recherche du premier Mouvement
    Dim w, a As String
    Dim rr, b As Integer
    Dim Found As Boolean
    Sheets!in.Activate
    Call FindLineWithStringNew("valeur")
    Found = False
    rr = FirstRow
    While Found = False
        rr = rr - 1
        Cells(rr, c).Select
        a = Cells(rr, c).Value
        b = Len(a)
        If ((b = 1 And a = " ") Or (rr = 1)) Then Found = True
        Wend
    FirstRow = rr
    End Sub
Sub FindLineWithStringNew(searched As String)
'   Recherche de la première ligne contenant String
'
'
Sheets!in.Activate
    Columns("A:A").Select
    Selection.Find(What:=searched, after:=ActiveCell, LookIn:=xlFormulas, _
        LookAt:=xlPart, SearchOrder:=xlByRows, SearchDirection:=xlNext, _
        MatchCase:=False).Activate
    FirstRow = ActiveCell.Row
    FirstCol = ActiveCell.Column
End Sub
    Sub findLastLineMvtCCNew(r As Integer, c As Integer)
'   la dernière ligne du mouvement est celle qui précède une ligne de longueur 1
    Dim w, a As String
    Dim rr, b As Integer
    Dim EndOfMvt As Boolean
Sheets!in.Activate
    EndOfMvt = False
    rr = r
    While EndOfMvt = False
        rr = rr + 1
        Cells(rr, c).Select
        a = Cells(rr, c).Value
        b = Len(a)
        If b = 1 Then EndOfMvt = True
    Wend
    LastRowMvt = rr - 1
    LastColMvt = c
    End Sub
Sub CreateSheetOut()
'
Sheets.Add
    ActiveSheet.Name = "Out"
    Range("A1").Select
    Application.CutCopyMode = False
    ActiveCell.FormulaR1C1 = "DateMvt"
    Range("B1").Select
    ActiveCell.FormulaR1C1 = "NMvt"
    Range("C1").Select
    ActiveCell.FormulaR1C1 = "DateVal"
    Range("D1").Select
    ActiveCell.FormulaR1C1 = "Montant"
    Range("E1").Select
    ActiveCell.FormulaR1C1 = "Libellé"
    End Sub

