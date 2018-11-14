{+------------------------------------------------------------
 | Unit UDoubleMatrixList
 |
 | Version: 1.0  Created: 15-3-01
 |               Last Modified: 15-3-01
 | Author : GenTypedLists Version 1.0
 | Project: General utilities
 | Description:
 |
 +------------------------------------------------------------}
Unit UDoubleMatrixList;
Interface

Uses Classes, AbstractTypedList, LargeArrays, SysUtils, Dialogs;

{.$DEFINE GTL_USEPOINTERS}
{$DEFINE GTL_USECOMPAREPROC}
{$DEFINE GTL_OWNSOBJECTS}

Type
  TDoubleMatrixSortCompare = Function( Const item1, item2: TDoubleMatrix ): Integer;

  TDoubleMatrixList = Class( TAbstractTypedList )
  Private
    Procedure Put( index: Integer; Const Item: TDoubleMatrix );
    Function  Get( index: Integer ): TDoubleMatrix;
    Procedure FreeItem( index: Integer );
    Procedure QuickSort(L, R: Integer; Compare: TDoubleMatrixSortCompare );
    procedure ReadAllData( Reader: TReader );
    procedure WriteAllData( Writer: TWriter );
  Protected
    procedure DefineProperties(Filer: TFiler); override;
  Public
    Function Clone: TDoubleMatrixList;
    Procedure Assign( source: TPersistent ); override;
    Function  Add(Const Item: TDoubleMatrix ): Integer;
    Procedure Insert(Index: Integer; Const Item: TDoubleMatrix);

    Procedure Clear; override;
    Procedure Delete(Index: Integer); override;
    Function  Detach(Index: Integer): TDoubleMatrix;

    Function LinearSearch( Const Item: TDoubleMatrix;
                           Compare: TDoubleMatrixSortCompare): Integer;
    Function BinarySearch( Const Item: TDoubleMatrix;
                           Compare: TDoubleMatrixSortCompare): Integer;
    Procedure Sort(Compare: TDoubleMatrixSortCompare);
    Property Items[Index: Integer]: TDoubleMatrix read Get write Put; default;
    Procedure WriteToTextFile( var f: TextFile ); Virtual;
    Function ReadDoubleMatrixListFromTextFile( var f: TextFile ): Integer; Virtual;
      {-Returns the number of tables read}
    Function SaveToStream( SaveStream: TStream ): Boolean; Virtual;
    Function LoadFromStream( LoadStream: TStream ): Boolean; Virtual;
  End;

{$IFDEF GTL_USECOMPAREPROC}
Function TDoubleMatrixCompare( Const item1, item2: TDoubleMatrix ): Integer;
{$ENDIF}

Implementation

uses
  uError;

const
  cMaxNrOfTables = 500;

{$IFDEF GTL_USEPOINTERS}
Type
    TDoubleMatrix_Ptr = ^TDoubleMatrix;
{$ENDIF}

{$IFDEF GTL_USECOMPAREPROC}
{+------------------------------------------------------------
 | Function TDoubleMatrixCompare
 |
 | Parameters :
 |   item1, item2: the two TDoubleMatrix items to compare
 | Returns    :
 |   0 if both items are equal, < 0 if item1 is smaller than item2,
 |   > 0 if item1 is larger than item2
 | Error Conditions: none
 | Created: 15-3-01 by GenTypedLists Version 1.0
 +------------------------------------------------------------}
Function TDoubleMatrixCompare( Const item1, item2: TDoubleMatrix ): Integer;
  Begin
    {  MODIFY THIS IMPLEMENTATION AS APPROPRIATE!  }
    {If item1 < item2 Then
      Result := -1
    Else If item1 > item2 Then
      Result := 1
    Else
      Result := 0;}
    Result := 0;
  End; { TDoubleMatrixCompare }
{$ENDIF}

{+-----------------------------
 | Methods of TDoubleMatrixList
 +----------------------------}

{+------------------------------------------------------------
 | Function TDoubleMatrixList.Clone: TDoubleMatrixList;
 |
 | Returns : a deep copy of this list instance.
 | Call method: static
 | Visibility : public
 | Description:
 |   Returns a new instance of the list class and copies the
 |   data from this list into it.
 | Error Conditions: none
 | Created: 15-3-01 by GenTypedLists Version 1.0
 +------------------------------------------------------------}
Function TDoubleMatrixList.Clone: TDoubleMatrixList;
  Begin
    Result := TDoubleMatrixList.Create;
    Result.Assign( self );
  End; { TDoubleMatrixList.Clone }

{+------------------------------------------------------------
 | Procedure TDoubleMatrixList.Assign
 |
 | Parameters :
 |   source: list instance to copy
 | Call method: virtual, overriden
 | Visibility : public
 | Description:
 |   Copies the data from the source into this instance, which
 |   is cleared first. This is a deep copy, unless the
 |   list contains a pointer type other than an object. If the
 |   list contains objects and owns
 |   the objects (GTL_OWNSOBJECTS defined) then the object 
 |   class MUST have a public Clone method, or a compiler error
 |   will result! If the list does not own the objects the copy
 |   is shallow, the new list stores references to the same objects
 |   as aList.
 | Error Conditions:
 |   If source is not of this list class the inherited Assign is
 |   called and that will raise an exception!
 | Created: 15-3-01 by GenTypedLists Version 1.0
 +------------------------------------------------------------}
Procedure TDoubleMatrixList.Assign( source: TPersistent );
  Var
    i: Integer;
    aList : TDoubleMatrixList;
  Begin
    If Assigned( source ) Then Begin
      If source Is TDoubleMatrixList Then Begin
        aList := TDoubleMatrixList( source );
        Clear;
        Capacity := aList.Count;
        For i := 0 To aList.LastIndex Do Begin
          {$IFDEF GTL_OWNSOBJECTS}
          case aList.Items[ i ].DescendantType of
            cDoubleArray: Insert( count, aList.Items[ i ].Clone );
            cDbleMtrxColindx: Insert( count, TDbleMtrxColindx(aList.Items[ i ]).Clone );
            cDbleMtrxColAndRowIndx: Insert( count, TDbleMtrxColAndRowIndx(aList.Items[ i ]).Clone );
          end; {-case}
          {$ELSE}
          Insert( count, aList.Items[ i ] );
          {$ENDIF}
        End; { For }
      End { If }
      Else
        inherited Assign( source );
    End; { If }
  End; { TDoubleMatrixList.Assign }

{+------------------------------------------------------------
 | Function TDoubleMatrixList.Add
 |
 | Parameters :
 |   Item to add. If this is a pointer type the list will store
 |   the pointer as is!
 | Returns    : the index of the item
 | Call method: static
 | Visibility : public
 | Description:
 |   Adds the passed item to the end of the list and returns the
 |   index of the item.
 | Error Conditions:
 |   We may run out of memory here, which will cause an exception.
 | Created: 15-3-01 by GenTypedLists Version 1.0
 +------------------------------------------------------------}
Function TDoubleMatrixList.Add(Const Item: TDoubleMatrix ): Integer;
  Begin
    Result := Count;
    Insert( Result, Item );
  End; { TDoubleMatrixList.Add }

{+------------------------------------------------------------
 | Procedure TDoubleMatrixList.Insert
 |
 | Parameters :
 |   Index: index of the item before which to insert the new item. 
 |          If Index >= Count the item will be appended to the list.
 |   Item : item to insert
 | Call method: static
 | Visibility : public
 | Description:
 |   Inserts the passed item into the list. If the data type is 
 |   larger than 4 bytes memory for the Item is allocated on the
 |   heap, the item is copied into it and the pointer is stored
 |   in the list, otherwise the data is stored directly, typecast
 |   to a pointer.
 | Error Conditions: 
 |   We may run out of memory here, which will cause an exception.
 | Created: 15-3-01 by GenTypedLists Version 1.0
 +------------------------------------------------------------}
Procedure TDoubleMatrixList.Insert(Index: Integer; Const Item: TDoubleMatrix);
  Begin
    If Index > Count Then 
      Index := Count;
    Storage.Insert( index, Nil );
    Put( index, Item );
  End; { TDoubleMatrixList.Insert }

{+------------------------------------------------------------
 | Procedure TDoubleMatrixList.Clear
 |
 | Parameters : none
 | Call method: virtual, overriden
 | Visibility : public
 | Description:
 |   Clears the list, freeing memory for the items if necessary.
 | Error Conditions: none
 | Created: 15-3-01 by GenTypedLists Version 1.0
 +------------------------------------------------------------}
Procedure TDoubleMatrixList.Clear;
  {$IFDEF GTL_OWNSOBJECTS} {$DEFINE GTL_MUSTFREE} {$ENDIF}
  {$IFDEF GTL_USEPOINTERS} {$DEFINE GTL_MUSTFREE} {$ENDIF}
  {$IFDEF GTL_MUSTFREE} 
  Var
    i: Integer;
 {$ENDIF}
  Begin
    {$IFDEF GTL_MUSTFREE} 
      For i := LastIndex Downto 0 Do  
        FreeItem( i );
      {$UNDEF GTL_MUSTFREE }
    {$ENDIF} 
    inherited Clear;
  End; { TDoubleMatrixList.Clear }

{+------------------------------------------------------------
 | Procedure TDoubleMatrixList.FreeItem
 |
 | Parameters : index of item to free
 | Call method: static
 | Visibility : private
 | Description:
 |   Frees the memory for the item at index, if required.
 | Error Conditions: 
 |   A invalid index will raise a EListError exception!
 | Created: 15-3-01 by GenTypedLists Version 1.0
 +------------------------------------------------------------}
Procedure TDoubleMatrixList.FreeItem( index: Integer );
  {$IFDEF GTL_USEPOINTERS} 
  Var
    pitem: TDoubleMatrix_Ptr;
  {$ENDIF}
  Begin
    {$IFDEF GTL_OWNSOBJECTS}
      Items[ Index ].Free;
    {$ELSE}
      {$IFDEF GTL_USEPOINTERS} 
        pItem := TDoubleMatrix_Ptr( Storage[ Index ] );
        If Assigned( pItem ) Then 
          Dispose( pItem );
      {$ENDIF}
    {$ENDIF}
  End; { TDoubleMatrixList.FreeItem }

{+------------------------------------------------------------
 | Procedure TDoubleMatrixList.Delete
 |
 | Parameters :
 |   Index: index of item to delete.
 | Call method: virtual, overridden
 | Visibility : public
 | Description:
 |   Deletes the item at index from the list, freeing memory for
 |   it if necessary.
 | Error Conditions:
 |   A invalid index will raise a EListError exception!
 | Created: 15-3-01 by GenTypedLists Version 1.0
 +------------------------------------------------------------}
Procedure TDoubleMatrixList.Delete(Index: Integer);
  Begin
    FreeItem( Index );
    inherited Delete( Index );
  End; { TDoubleMatrixList.Delete }

{+------------------------------------------------------------
 | Function  TDoubleMatrixList.Detach
 |
 | Parameters : index of item to remove from the list
 | Returns    : the removed item
 | Call method: static
 | Visibility : public
 | Description:
 |   Returns the item at index and then deletes the entry from
 |   the list. If the list stores objects the object will of 
 |   course *not* be freed, the list relegates ownership of the
 |   data to the caller.
 | Error Conditions: 
 |   A invalid index will raise a EListError exception!
 | Created: 15-3-01 by GenTypedLists Version 1.0
 +------------------------------------------------------------}
Function  TDoubleMatrixList.Detach(Index: Integer): TDoubleMatrix; 
  Begin
    Result := Items[ Index ];
    {$IFDEF GTL_USEPOINTERS}
    Delete( index );
    {$ELSE}
    inherited Delete( index );
    {$ENDIF} 
  End; { TDoubleMatrixList.Detach }

{+------------------------------------------------------------
 | Procedure TDoubleMatrixList.Put
 |
 | Property   : Items ( write )
 | Call method: static
 | Visibility : private
 | Description:
 |   Frees memory for the item at index, if required, and stores
 |   the passed item in that slot of the list. If the data type is
 |   larger than 4 bytes memory for the Item is allocated on the 
 |   heap, the item is copied into it and the pointer is stored
 |   in the list, otherwise the data is stored directly, typecast
 |   to a pointer.
 | Error Conditions:
 |   A invalid index will raise a EListError exception!
 | Created: 15-3-01 by GenTypedLists Version 1.0
 +------------------------------------------------------------}
Procedure TDoubleMatrixList.Put( index: Integer; Const Item: TDoubleMatrix );
  {$IFDEF GTL_USEPOINTERS} 
  Var
    pTemp: TDoubleMatrix_Ptr;
  {$ENDIF} 
  Begin
    FreeItem( index );
    {$IFDEF GTL_USEPOINTERS} 
    New( pTemp );
    Try
      pTemp^ := Item;
      Storage.Items[ index ]:= pTemp;
    Except
      Dispose( pTemp );
      raise
    End;
    {$ELSE} 
    Storage.Items[ Index ]:= Pointer( Item );
    {$ENDIF}
  End; { TDoubleMatrixList.Put }

{+------------------------------------------------------------
 | Function  TDoubleMatrixList.Get
 |
 | Property   : Items ( read )
 | Call method: static
 | Visibility : private
 | Description:
 |   Returns the item at Index in the list.
 | Error Conditions: 
 |   A invalid index will raise a EListError exception!
 | Created: 15-3-01 by GenTypedLists Version 1.0
 +------------------------------------------------------------}
Function  TDoubleMatrixList.Get( index: Integer ): TDoubleMatrix;
  Begin
    {$IFDEF GTL_USEPOINTERS}
    Result := TDoubleMatrix_Ptr( Storage.Items[ index ] )^
    {$ELSE} 
    Result := TDoubleMatrix( Storage.Items[ index ] );
    {$ENDIF}
  End; { TDoubleMatrixList.Get }

{+------------------------------------------------------------
 | Function TDoubleMatrixList.LinearSearch
 |
 | Parameters :
 |   Item: item to search for
 |   Compare: compare method to use
 | Returns    : the index of the item, or -1, if the item is not
 |              found.
 | Call method: static
 | Visibility : public
 | Description:
 |   Performs a linear search over the list and stops at the first
 |   item that matches the passed one.
 | Error Conditions: none
 | Created: 15-3-01 by GenTypedLists Version 1.0
 +------------------------------------------------------------}
Function TDoubleMatrixList.LinearSearch( Const Item: TDoubleMatrix; 
                       Compare: TDoubleMatrixSortCompare): Integer;
  Var
    i: Integer;
  Begin
    Assert( Assigned( Compare ));
    Result := -1;
    For i := 0 To LastIndex Do Begin 
      If Compare( Item, Items[ i ] ) = 0 Then Begin
        Result := i;
        Break;
      End; { If }
    End; { For }
  End; { TDoubleMatrixList.LinearSearch }

{+------------------------------------------------------------
 | Function TDoubleMatrixList.BinarySearch
 |
 | Parameters :
 |   Item: item to search for
 |   Compare: compare method to use
 | Returns    : the index of the item, or -1, if the item is not
 |              found.
 | Call method: static
 | Visibility : public
 | Description:
 |   Performs a binary search over the list and stops at the first
 |   item that matches the passed one. The list needs to be sorted
 |   for this kind of find to work and the same Compare function 
 |   needs to be used for sort and search! If the list contains duplicate
 |   items binary search will find one of them but not necessarily 
 |   the first!
 |   The implementation has not been optimized for speed in any way!
 | Error Conditions: none
 | Created: 15-3-01 by GenTypedLists Version 1.0
 +------------------------------------------------------------}
Function TDoubleMatrixList.BinarySearch( Const Item: TDoubleMatrix;
                       Compare: TDoubleMatrixSortCompare): Integer;
  Var
    first, last, pivot, res: Integer;
  Begin
    Assert( Assigned( Compare ));
    Result := -1;
    If count = 0 Then Exit;

    first := 0;
    last  := LastIndex;
    Repeat 
      pivot := ( first + last ) div 2;
      res := Compare( Item, Items[ pivot ] );
      If res = 0 Then Begin
        { Found the item, return its index and exit. }
        Result := pivot;
        Break;
      End { If } 
      Else If res > 0 Then Begin 
        { Item is larger than item at pivot }
        first := pivot + 1;
      End { If }
      Else Begin
        { Item is smaller than item at pivot } 
        last := pivot - 1;
      End;
    Until last < first;
  End; { TDoubleMatrixList.BinarySearch }

Procedure TDoubleMatrixList.QuickSort(L, R: Integer; Compare: TDoubleMatrixSortCompare );
  Var
    I, J: Integer;
    P: TDoubleMatrix;
  Begin
    Repeat
      I := L;
      J := R;
      P := Items[(L + R) shr 1];
      Repeat
        While Compare(Items[I], P) < 0 Do Inc(I);
        While Compare(Items[J], P) > 0 Do Dec(J);
        If I <= J Then
        Begin
          Exchange( I, J );
          Inc(I);
          Dec(J);
        End;
      Until I > J;
      If L < J Then QuickSort(L, J, Compare);
      L := I;
    Until I >= R;
  End; { TDoubleMatrixList.QuickSort }

{+------------------------------------------------------------
 | Procedure TDoubleMatrixList.Sort
 |
 | Parameters : function to use to compare items
 | Call method: static
 | Visibility : public
 | Description:
 |   Performs a quicksort on the list. The sort code is modified
 |   from TList.Sort.
 | Error Conditions: none
 | Created: 15-3-01 by GenTypedLists Version 1.0
 +------------------------------------------------------------}
Procedure TDoubleMatrixList.Sort(Compare: TDoubleMatrixSortCompare);
  Begin
    Assert( Assigned( Compare ));
    If Count > 1 Then 
      QuickSort(0, LastIndex, Compare);
  End; { TDoubleMatrixList.Sort }

Function TDoubleMatrixList.ReadDoubleMatrixListFromTextFile( var f: TextFile ): Integer;
var
  NrOfDoubleMatrices, i, iTableType, InitialCount: Integer;
begin
  WriteToLogFile( 'TDoubleMatrixList: Reading TDoubleMatrices (or descendants) from text-file.' );
  InitialCount := Count;
  Try
    Readln( f, NrOfDoubleMatrices );
    WriteToLogFileFmt( 'NrOfDoubleMatrices= %d', [NrOfDoubleMatrices] );

    for i:=0 to NrOfDoubleMatrices-1 do begin
      Readln( f, iTableType );
      case iTableType of {-Zie corresponderende code in 'ExtParU.pas'}
        Ord( cDoubleArray ): begin
             WriteToLogFileFmt( 'Reading matrix %d; type: TDoubleMatrix.', [i+1] );
             Add( TDoubleMatrix.InitialiseFromTextFile( f, nil ) );
           end;
        Ord( cDbleMtrxColindx ): begin
             WriteToLogFileFmt( 'Reading matrix %d; type: TDbleMtrxColIndx.', [i+1] );
             Add( TDbleMtrxColindx.InitialiseFromTextFile( f, nil ) );
           end;
        Ord( cDbleMtrxColAndRowIndx ): begin
	     WriteToLogFileFmt( 'Reading matrix %d; type: TDbleMtrxColAndRowIndx.', [i+1] );
	     Add( TDbleMtrxColAndRowIndx.InitialiseFromTextFile( f, nil ) );
           end;
      else
        WriteToLogFileFmt( 'Unknown table type: $d.', [iTableType] );
        raise Exception.Create( 'Unknown table type ' +
                            '"in TDoubleMatrixList.ReadDoubleMatrixsFromTextFile".' )
      end;
    end;
  Except
    On E: Exception do
      HandleError( E.Message, true );
  end;
  Result := ( Count - InitialCount );
end;

Procedure TDoubleMatrixList.WriteToTextFile( var f: TextFile );
var
  i: Integer;
begin
  Writeln( f, Count ); {=NrOfDoubleMatrices}
  for i:=0 to LastIndex do begin
    with Items[ i ] do begin
      WriteDescendantTypeToTextFile( f );
      WriteToTextFile( f, ' ' );
    end; {-with}
  end; {-for i}
end;

Function TDoubleMatrixList.SaveToStream( SaveStream: TStream ): Boolean;
var
  i: Integer;
begin
  Result := true;
  Try
    SaveStream.WriteData( Count, Sizeof( Integer ) ); {=NrOfDoubleMatrices}
    i := 0;
    while Result and ( i <= LastIndex ) do begin
      SaveStream.WriteData(  Integer( Items[ i ].DescendantType ), SizeOf( Integer ) );
      Result := Items[ i ].SaveToStream( SaveStream );
      Inc( i );
    end; {-while}
  Except
    Result := false;
  End;
end;

Function TDoubleMatrixList.LoadFromStream( LoadStream: TStream ): Boolean;
var
  i, NrOfDoubleMatrices, iTableType: Integer;
begin
  Result := true;
  Try
    clear;
    LoadStream.ReadData( NrOfDoubleMatrices, SizeOf( Integer ) );
    i := 0;
    while Result and ( i <= NrOfDoubleMatrices-1 )  do begin
      LoadStream.ReadData( iTableType, SizeOf( Integer ) );
      case TDoubleArrayDescendant( iTableType ) of {-Zie corresponderende code in 'ExtParU.pas'}
        cDoubleArray: Add( TDoubleMatrix.Create( 0, 0, nil ) );
        cDbleMtrxColindx: Add( TDbleMtrxColindx.Create( 0, 0, nil ) );
        cDbleMtrxColAndRowIndx: Add( TDbleMtrxColAndRowIndx.Create( 0, 0, nil ) );
      else
        raise Exception.Create( 'Unknown table type ' +
                            '"in TDoubleMatrixList.ReadDoubleMatrixsFromTextFile".' )
      end;
      Result := Items[ i ].LoadFromStream( LoadStream );
      Inc( i );
    end; {-while}
  Except
    Result := false;
  end;
end;

Procedure TDoubleMatrixList.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  { Define new properties and reader/writer methods }
  Filer.DefineProperty('DoubleMatrixListData', ReadAllData, WriteAllData, True );
end;

procedure TDoubleMatrixList.WriteAllData( Writer: TWriter );
var
  i: Integer;
begin
  Writer.WriteInteger( Count );
  for i:=0 to Count-1 do begin
    Writer.WriteInteger ( Integer( Items[ i ].DescendantType ) );
    Items[ i ].WriteAllData( Writer );
  end;
end;

procedure TDoubleMatrixList.ReadAllData( Reader: TReader );
var
  i, NrOfDoubleMatrices, iTableType: Integer;
begin
  clear;
  Try
    NrOfDoubleMatrices := Reader.ReadInteger;
    for i:=0 to NrOfDoubleMatrices-1 do begin
      iTableType := Reader.ReadInteger;
      case TDoubleArrayDescendant( iTableType ) of {-Zie corresponderende code in 'ExtParU.pas'}
        cDoubleArray: Add( TDoubleMatrix.Create( 0, 0, nil ) );
        cDbleMtrxColindx: Add( TDbleMtrxColindx.Create( 0, 0, nil ) );
        cDbleMtrxColAndRowIndx: Add( TDbleMtrxColAndRowIndx.Create( 0, 0, nil ) );
      else
        raise Exception.Create( 'Unknown table type ' +
                            '"in TDoubleMatrixList.ReadDoubleMatrixsFromTextFile".' )
      end;
      Items[ i ].ReadAllData( Reader );
    end; {-for}
  Except
    On E: Exception do
      MessageDlg( E.Message, mtError, [mbOk], 0);
  end;
end;

begin
 with FormatSettings do begin {-Delphi XE6}
   DecimalSeparator := '.';
 end;
End { UDoubleMatrixList }.

