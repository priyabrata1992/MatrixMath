(*
              UnitName : MatrixMath.
              Coder : Priyabrata.
              Function : Performs Common Matrix Arithmatic.
              Release Version : 1.
              Credits :   Wikipedia, Gauss & Jordan for the Great Algorithm
	            Known Bugs : Matrices over (6x6) gives rise to Floating point errors
			        with Inversion Procedure. I think I can fix this in future releases.
              Other Procedures work as they are intended to.
*)

(*
  Note : This is an open source library. Hence feel free to use it in
         any way you want & can modify it too. How ever, above Header
         cannot be removed/deleted from this unit.
*)

unit MatrixMath;

interface

uses
Math, Sysutils, Windows;

type
  TMatrix = record
     Row : Integer;
     Col : Integer;
     Matrix : Array OF Array OF Double;
  End;

Procedure InitMatrix(RowLength, ColumnLength : Integer; var Matrix : TMatrix);
Procedure DestroyMatrix(var Matrix : TMatrix);
Procedure CopyMatrix(SourceMatrix :TMatrix; var Destination : TMatrix);
Procedure CreateIdentityMatrix(RowSize, ColumnSize : Integer; var Identity : TMatrix);
Procedure CreateNullMatrix(RowSize, ColumnSize : Integer; var NullMat : TMatrix);
Procedure AddMatrix(MatrixA, MatrixB : TMatrix; var Sum : TMatrix);
Procedure SubtractMatrix(MatrixA, MatrixB : TMatrix; var Dif : TMatrix);
Procedure MatrixScalarMul(MatrixA : TMatrix; Scalar_Value : Int64; var Product : TMatrix);
Procedure MatrixScalarMulMod(MatrixA : TMatrix; Scalar_Value : Int64; Modulus : Int64; var Product : TMatrix);
Procedure MatrixTranspose(SourceMatrix : TMatrix; var Transpose : TMatrix);
Procedure MatrixMultiply(MatrixA, MatrixB : TMatrix; var Product : TMatrix);
Procedure MatrixDeterminant(TargetMatrix : TMatrix; var determinant : Extended);
Procedure MatrixInverse(TargetMatrix : TMatrix; var Inverse : TMatrix; var Singular : Boolean);
Procedure MatrixModInverse(TargetMatrix : TMatrix; Modulus : Int64; var Inverse : Tmatrix);
Procedure MatrixMulMod(MatrixA, MatrixB : TMatrix; Modulus : Int64; var Product : TMatrix);
Procedure IsIdentityMatrix(Target : TMatrix; var Identity : Boolean);
Procedure Random_Non_Sigular_Matrix(Row, Column : Integer; var RandomMatrix : TMatrix);
Procedure CompareMatrix(MatrixA : TMatrix; MatrixB : TMatrix; var Result : Byte);
Function  ModularInverse(a,n : Int64):Int64;

implementation

Procedure InitMatrix(RowLength, ColumnLength : Integer; var Matrix : TMatrix);
Begin
  Matrix.Row := RowLength;
  Matrix.Col := ColumnLength;
  SetLength(Matrix.Matrix, Matrix.Row, Matrix.Col);
End;

Procedure DestroyMatrix(var Matrix : TMatrix);
Begin
  Matrix.Row := 0;
  Matrix.Col := 0;
  Matrix.Matrix := nil;
End;

Procedure CopyMatrix(SourceMatrix :TMatrix; var Destination : TMatrix);
var
i, j : integer;
Begin
  InitMatrix(SourceMatrix.Row, SourceMatrix.Col, Destination);
  for i := 0 to SourceMatrix.Row - 1 do
   Begin
     for j := 0 to SourceMatrix.Col - 1 do
      Begin
        Destination.Matrix[i, j] := SourceMatrix.Matrix[i, j];
      End;
   End;
End;

Procedure CreateIdentityMatrix(RowSize, ColumnSize : Integer; var Identity : TMatrix);
var
i, j : integer;
Begin
  if (RowSize = ColumnSize) then
  Begin
    InitMatrix(RowSize, ColumnSize, Identity);
  End;
  for i := 0 to RowSize - 1 do
   Begin
     for j := 0 to ColumnSize - 1 do
      Begin
        if (i = j) then
           Identity.Matrix[i,j] := 1
        else
           Identity.Matrix[i,j] := 0;
      End;
   End;
End;

Procedure CreateNullMatrix(RowSize, ColumnSize : Integer; var NullMat : TMatrix);
var
i, j : integer;
Begin
  if (RowSize = ColumnSize) then
  Begin
    InitMatrix(RowSize, ColumnSize, NullMat);
  End;
  for i := 0 to RowSize - 1 do
   Begin
     for j := 0 to ColumnSize - 1 do
      Begin
        NullMat.Matrix[i,j] := 0;
      End;
   End;
End;

Procedure AddMatrix(MatrixA, MatrixB : TMatrix; var Sum : TMatrix);
var
i, j, r, c : Integer;
Begin
  if (MatrixA.Row = MatrixB.Row) then
  Begin
    if (MatrixA.Col = MatrixB.Col) then
    Begin
      InitMatrix(MatrixA.Row, MatrixA.Col, Sum);
      r := MatrixA.Row;
      c := MatrixA.Col;
      for i := 0 to r-1 do
       Begin
         for j := 0 to c-1 do
          Begin
            Sum.Matrix[i,j] := MatrixA.Matrix[i,j] + MatrixB.Matrix[i,j];
          End;
       End;
    End;
  End;
End;

Procedure SubtractMatrix(MatrixA, MatrixB : TMatrix; var Dif : TMatrix);
var
i, j, r, c : Integer;
Begin
  if (MatrixA.Row = MatrixB.Row) then
  Begin
    if (MatrixA.Col = MatrixB.Col) then
    Begin
      InitMatrix(MatrixA.Row, MatrixA.Col, Dif);
      r := MatrixA.Row;
      c := MatrixA.Col;
      for i := 0 to r-1 do
       Begin
         for j := 0 to c-1 do
          Begin
            Dif.Matrix[i,j] := MatrixA.Matrix[i,j] - MatrixB.Matrix[i,j];
          End;
       End;
    End;
  End;
End;

Procedure MatrixScalarMul(MatrixA : TMatrix; Scalar_Value : Int64; var Product : TMatrix);
var
i, j : Integer;
Begin
 InitMatrix(MatrixA.Row, MatrixA.Col, Product);
 for i := 0 to MatrixA.Row-1 do
  Begin
    for j := 0 to MatrixA.Col-1 do
     Begin
       Product.Matrix[i,j] := MatrixA.Matrix[i,j] * Scalar_Value;
     End;
  End;
End;

Procedure MatrixScalarMulMod(MatrixA : TMatrix; Scalar_Value : Int64; Modulus : Int64; var Product : TMatrix);
var
i, j, rem : Integer;
tmp1 : String;
tmp : Double;
tmp2 : Int64;
Begin
 InitMatrix(MatrixA.Row, MatrixA.Col, Product);
 for i := 0 to MatrixA.Row-1 do
  Begin
    for j := 0 to MatrixA.Col-1 do
     Begin
       Product.Matrix[i,j] := MatrixA.Matrix[i,j] * Scalar_Value;
       tmp := Product.Matrix[i,j];
       tmp := Round(tmp);
       tmp1 := FloatToStr(tmp);
       tmp2 := StrToInt64(tmp1);
       rem  := Tmp2 mod Modulus;
       Tmp1 := IntToStr(Rem);
       Product.Matrix[i,j] := StrToFloat(Tmp1);
     End;
  End;
End;

Procedure MatrixTranspose(SourceMatrix : TMatrix; var Transpose : TMatrix);
var
i, j : Integer;
Begin
  if (SourceMatrix.Row <> SourceMatrix.Col) then
  Begin
    InitMatrix(0, 0, Transpose);
    exit;
  End;
  InitMatrix(SourceMatrix.Row, SourceMatrix.Col, Transpose);
  for i := 0 to SourceMatrix.Row - 1 do
   Begin
     for j := 0 to SourceMatrix.Col - 1 do
      Begin
        Transpose.Matrix[i,j] := Sourcematrix.Matrix[j,i];
      End;
   End;
End;

Procedure MatrixMultiply(MatrixA, MatrixB : TMatrix; var Product : TMatrix);
var
i, j, k : integer;
Begin
  InitMatrix(MatrixA.Row, MatrixB.Col, Product);
  for i := 0 to MatrixA.Row - 1 do
   Begin
     for j := 0 to MatrixB.Col - 1 do
      Begin
        Product.Matrix[i,j] := 0;
        for k := 0 to MatrixA.Col - 1 do
         Begin
           Product.Matrix[i, j] := Product.Matrix[i,j] + (MatrixA.Matrix[i,k] * MatrixB.Matrix[k,j]);
         End;
      End;
   End;
End;

Procedure MatrixDeterminant(TargetMatrix : TMatrix; var determinant : Extended);
var
mCopy : TMatrix;
i, j, k : integer;
det, fac,tmp : double;
Begin
  det := 1;
  CopyMatrix(TargetMatrix, mCopy);
  for i := 0 to mCopy.Row - 1 do
   Begin
     if (mCopy.Matrix[i, i] = 0) then
     Begin
       for j := i + 1 to mCopy.Row -1 do
        Begin
          if (mCopy.Matrix[j,i] <> 0) then
          Begin
            for k := 0 to mCopy.Row - 1 do
             Begin
               tmp := mCopy.Matrix[i,k];
               mCopy.Matrix[i,k] := mCopy.Matrix[j,k];
               mCopy.Matrix[j,k] := tmp;
             End;
             det := -det;
             break;
          End;
        End;
     End;
     if (mCopy.Matrix[i,i] = 0) then
     Begin
       determinant := 0;
       exit;
     End;
     for j := i+1 to mCopy.Row -1 do
      Begin
        if ((mCopy.Matrix[j, i]) <> 0) then
        Begin
          fac := mCopy.Matrix[j,i] / mCopy.Matrix[i,i];
          for k := i to mCopy.Row - 1 do
           Begin
             mCopy.Matrix[j, k] :=  mCopy.Matrix[j, k] - (fac * mCopy.Matrix[i,k]);
           End;
        End;
      End;
   End;
   for i := 0 to mCopy.Row - 1 do
    Begin
      det := det * mCopy.Matrix[i,i];
    End;
   determinant := det;
   DestroyMatrix(mCopy);
End;

Procedure MatrixInverse(TargetMatrix : TMatrix; var Inverse : TMatrix; var Singular : Boolean);
var
IdentityMatrix, tCopy : TMatrix;
i, j, k : integer;
tmp, det : Extended;
Begin
  MatrixDeterminant(TargetMatrix, det);
  if (det = 0) then
  Begin
    Singular := True;
    CreateNullMatrix(TargetMatrix.Row, TargetMatrix.Col, IdentityMatrix);
    CopyMatrix(IdentityMatrix, Inverse);
    exit;
  End;
  CopyMatrix(TargetMatrix, tCopy);
  if (TargetMatrix.Row <> TargetMatrix.Col) then
  Begin
    Exit;
  End;
  InitMatrix(TargetMatrix.Row, TargetMatrix.Col, Inverse);
  CreateIdentityMatrix(TargetMatrix.Row, TargetMatrix.Col, IdentityMatrix);
  for i := 0 to TargetMatrix.Row - 1 do
   Begin
     tmp := tCopy.Matrix[i,i];
     for j := 0 to TargetMatrix.Row - 1 do
      Begin
      try
         tCopy.Matrix[i,j] := (tCopy.Matrix[i,j] / tmp);
         IdentityMatrix.Matrix[i,j] := (IdentityMatrix.Matrix[i,j] / tmp);
       except
         on E:Exception do
         Begin
           Singular := True;
           CreateNullMatrix(TargetMatrix.Row, TargetMatrix.Col, IdentityMatrix);
           CopyMatrix(IdentityMatrix, Inverse);
           exit;
         End;
       End;
      End;
     for k := 0 to TargetMatrix.Row - 1 do
      Begin
        tmp := tCopy.Matrix[k, i];
        for j := 0 to TargetMatrix.Row - 1 do
         Begin
           if (i = k) then
           Begin
             break;
           End
           else
           Begin
             tCopy.Matrix[k,j] := (tCopy.Matrix[k, j] - (tCopy.matrix[i, j] * tmp));
             IdentityMatrix.Matrix[k, j] := (IdentityMatrix.Matrix[k, j] - (IdentityMatrix.Matrix[i, j] * tmp));
           End;
         End;
      End;
   End;
   CopyMatrix(IdentityMatrix, Inverse);
   Singular := False;
End;

Function ModularInverse(a,n : Int64):Int64;
var
b, x, lastx, tmp, quot : int64;
Begin
  b := n;
  x := 0;
  lastx := 1;
  while (b <> 0) do
  Begin
    tmp  := b;
    quot := a div b;
    b    := a mod b;
    a    := tmp;
    tmp  := x;
    x    := lastx - (quot * x);
    lastx:= tmp;
  End;
  if (lastx < 0) then
  Begin
    lastx := lastx + n;
  End;
  result := lastx;
End;

Procedure MatrixModInverse(TargetMatrix : TMatrix; Modulus : Int64; var Inverse : Tmatrix);
var
det : Extended;
sDet : String;
iDet, inverse_Det : int64;
matInv, Adjoint : TMatrix;
Singular : Boolean;
i,j : Integer;
Begin
  MatrixDeterminant(TargetMatrix, Det);
  Det := Round(Det);
  sDet := FloatToStr(Det);
  iDet := StrToInt64(sDet);
  inverse_det := ModularInverse(iDet, Modulus);
  MatrixInverse(TargetMatrix, matInv, Singular);
  If Not bool(Singular) Then
  Begin
    InitMatrix(TargetMatrix.Row, TargetMatrix.Col, Adjoint);
    MatrixScalarMul(MatInv, iDet, Adjoint);
    MatrixScalarMulMod(Adjoint, Inverse_Det, Modulus, Inverse);
  End;
  for i := 0 to Inverse.Row - 1 do
   Begin
     for j := 0 to Inverse.Col - 1 do
      Begin
        if (Inverse.Matrix[i,j] < 0) then
        Begin
          Inverse.Matrix[i, j] := Modulus + Inverse.Matrix[i, j];
        End;
      End;
   End;
End;

Procedure MatrixMulMod(MatrixA, MatrixB : TMatrix; Modulus : Int64; var Product : TMatrix);
var
i, j, k, tmp2, rem : integer;
tmp1 : String;
Begin
  InitMatrix(MatrixA.Row, MatrixB.Col, Product);
  for i := 0 to MatrixA.Row - 1 do
   Begin
     for j := 0 to MatrixB.Col - 1 do
      Begin
        Product.Matrix[i,j] := 0;
        for k := 0 to MatrixA.Col - 1 do
         Begin
           Product.Matrix[i, j] := Product.Matrix[i,j] + (MatrixA.Matrix[i,k] * MatrixB.Matrix[k,j]);
         End;
         Tmp1 := FloatToStr(Product.Matrix[i, j]);
         Tmp2 := StrToInt(Tmp1);
         Rem := Tmp2 mod Modulus;
         Tmp1 := IntToStr(Rem);
         Product.Matrix[i, j] := StrToFloat(Tmp1);
      End;
   End;
End;

Procedure CompareMatrix(MatrixA : TMatrix; MatrixB : TMatrix; var Result : Byte);
var
i, j : integer;
Begin
 if (MatrixA.Row = MatrixB.Row) then
 Begin
   if (MatrixA.Col = MatrixB.Col) then
   Begin
    for i := 0 to MatrixA.Row - 1 do
     Begin
       for j := 0 to MatrixA.Col - 1 do
        Begin
          if (MatrixA.Matrix[i,j] <> MatrixB.Matrix[i,j]) then
          Begin
            Result := 1;
            exit;
          End
          else
          Begin
            Result := 0;
          End;
        End;
     End;
   End
   else
   Begin
     result := 1;
   End;
 End
 else
 Begin
   Result := 1;
 End;
End;

Procedure Random_Non_Sigular_Matrix(Row, Column : Integer; var RandomMatrix : TMatrix);
var
i, j : integer;
det : Extended;
Begin
  InitMatrix(Row, Column, RandomMatrix);
  Randomize;
  RandSeed := GetTickCount xor $DEADC0DE;
  repeat
    det := 0;
    for i := 0 to Row - 1 do
     Begin
      for j := 0 to Column - 1 do
       Begin
         RandomMatrix.Matrix[i,j] := Random($F);
       End;
     End;
    MatrixDeterminant(RandomMatrix, det);
  until
    det <> 0;
End;

Procedure IsIdentityMatrix(Target : TMatrix; var Identity : Boolean);
var
i, j, r, c : integer;
tmp : extended;
Begin
  Identity := True;
  r := Target.Row - 1;
  c := Target.Col - 1;
  tmp := Target.Matrix[0,0];
  if (tmp = 0) then
  Begin
    Identity := false;
    exit;
  End;
  for i := 0 to r do
   Begin
     for j := 0 to c do
      Begin
        if (i = j) then
        Begin
          if (Target.Matrix[i,j] <> tmp) then
          Begin
            Identity := false;
            exit;
          End;
        End
        else
        Begin
          if (Target.Matrix[i, j] <> 0)  then
          Begin
            Identity := False;
            exit;
          End;
        End;
      End;
   End;
End;

end.
