unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Menus,
  Xml.XMLDoc, Xml.XMLIntf, Vcl.ExtCtrls,
  ShellAPI,
  System.IOUtils, Vcl.Grids,
  StrUtils, System.Generics.Collections;

type
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    Exit1: TMenuItem;
    TreeView1: TTreeView;
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    SaveAs1: TMenuItem;
    Splitter1: TSplitter;
    View1: TMenuItem;
    ToggleRawView1: TMenuItem;
    PanelDetailView: TPanel;
    GroupBoxAttributes: TGroupBox;
    GroupBoxText: TGroupBox;
    GroupBoxChildren: TGroupBox;
    StringGridAttributes: TStringGrid;
    btnAddAttribute: TButton;
    btnDeleteAttribute: TButton;
    btnEditAttribute: TButton;
    MemoNodeText: TMemo;
    LabelSelectNode: TLabel;
    BtnAddChild: TButton;
    BtnEditChild: TButton;
    BtnDeleteChild: TButton;
    ListBoxChildren: TListBox;
    btnEditNodeText: TButton;
    btnDeleteNodeText: TButton;
    procedure New1Click(Sender: TObject);

    procedure LoadXMLFile(var FileName: string);

    procedure Open1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure SaveAs1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

    procedure LoadXMLIntoTree(Doc: IXMLDocument);

    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
    procedure ToggleRawView1Click(Sender: TObject); // Allow file darg-drop

    procedure PopulatePanel(TreeNode: TTreeNode);
    procedure ClearPanel;
    procedure TreeView1Click(Sender: TObject);

  private
    CurrentFileName: string;
    Modified: Boolean;

    FXMLDoc: IXMLDocument;

    NodeMap: TDictionary<TTreeNode, IXMLNode>;

    ShowRaw: Boolean;
    IsNodeSelected: Boolean;
    HasNodeChildren: Boolean;

  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Exit1Click(Sender: TObject);
begin
  NodeMap.Free;
  Close;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  dlg: TForm;
  IsTempFile: Boolean;
begin
  IsTempFile := (CurrentFileName <> '') and TPath.GetTempPath.StartsWith
    (ExtractFilePath(CurrentFileName));

  if Modified then
  begin
    dlg := CreateMessageDialog('You have unsaved changes. Save before exiting?',
      mtConfirmation, [mbYes, mbNo, mbCancel]);
    dlg.Position := poDesigned;
    dlg.Left := Self.Left + (Self.Width - dlg.Width) div 2;
    dlg.Top := Self.Top + (Self.Height - dlg.Height) div 2;

    case dlg.ShowModal of
      mrYes:
        begin
          Save1Click(nil);
          CanClose := not Modified;
          if CanClose and IsTempFile and FileExists(CurrentFileName) then
            DeleteFile(CurrentFileName);
        end;
      mrNo:
        begin
          CanClose := True;
          // If user discards changes and it's a temp file, delete it
          if IsTempFile and FileExists(CurrentFileName) then
            DeleteFile(CurrentFileName);
        end;
      mrCancel:
        CanClose := False;
    end;
    dlg.Free;
  end
  else
  begin
    CanClose := True;
    // No unsaved changes but if it's temp file, delete it (no longer needed)
    if IsTempFile and FileExists(CurrentFileName) then
      DeleteFile(CurrentFileName);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ShowRaw := True; // Raw view is default --- for now
  IsNodeSelected := False;
  HasNodeChildren := False;
  NodeMap := TDictionary<TTreeNode, IXMLNode>.Create;

  StringGridAttributes.ColCount := 2;
  StringGridAttributes.Cells[0, 0] := 'Attribute';
  StringGridAttributes.Cells[1, 0] := 'Value';


  DragAcceptFiles(Handle, True); // Allow file drag-drop
  New1Click(nil); // simulate clicking File > New on startup
end;

procedure TForm1.Memo1Change(Sender: TObject);
begin
  Modified := True;

  if not ShowRaw then Exit;

  try
    FXMLDoc.LoadFromXML(Memo1.Text);
    FXMLDoc.Active := True;

    TreeView1.Items.Clear;
    NodeMap.Clear; // Clear the dict
    LoadXMLIntoTree(FXMLDoc); // Rebuild tree and dict
  except
    // ignore invalid XML silently
  end;
end;


procedure TForm1.New1Click(Sender: TObject);
var
  TempFile: string;
begin
  TempFile := TPath.Combine(TPath.GetTempPath, TPath.GetRandomFileName
    + '.xml');

  // Create minimal XML content
  var
  XMLContent := TStringList.Create;
  try
    XMLContent.Text := '<?xml version="1.0" encoding="UTF-8"?>' + sLineBreak +
      '<root>' + sLineBreak + '<!-- Add your XML content here -->' + sLineBreak
      + '</root>';
    XMLContent.SaveToFile(TempFile);
  finally
    XMLContent.Free;
  end;

  CurrentFileName := TempFile;

  LoadXMLFile(CurrentFileName);

  Modified := False;
  Caption := 'XML Editor - New Document (Unsaved)';
end;

procedure TForm1.LoadXMLFile(var FileName: string);
begin
  FXMLDoc := TXMLDocument.Create(nil);
  FXMLDoc.LoadFromFile(FileName);
  FXMLDoc.Active := True;

  if ShowRaw then
    Memo1.Text := FXMLDoc.Xml.Text
  else
    Memo1.Clear;
    PopulatePanel(Nil);

  ClearPanel;
  NodeMap.Clear;
  TreeView1.Items.Clear;
  LoadXMLIntoTree(FXMLDoc);

  Modified := False;
  Caption := 'XML Editor - ' + FileName;
end;

procedure TForm1.Open1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin

    CurrentFileName := OpenDialog1.FileName;

    LoadXMLFile(CurrentFileName);

  end;
end;

procedure TForm1.Save1Click(Sender: TObject);
var
  IsTempFile: Boolean;
begin
  // Check if CurrentFileName is inside the temp folder
  IsTempFile := (CurrentFileName <> '') and TPath.GetTempPath.StartsWith
    (ExtractFilePath(CurrentFileName));

  if IsTempFile or (CurrentFileName = '') then
  begin
    // Redirect to Save As to force user to pick a real location
    SaveAs1Click(Sender);
    Exit;
  end;

  // Normal save
  Memo1.Lines.SaveToFile(CurrentFileName);
  Caption := 'XML Editor - ' + CurrentFileName;
  Modified := False;
end;

procedure TForm1.SaveAs1Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    CurrentFileName := SaveDialog1.FileName;
    Memo1.Lines.SaveToFile(CurrentFileName);
    Modified := False;
    Caption := 'XML Editor - ' + CurrentFileName;
  end;
end;

procedure TForm1.LoadXMLIntoTree(Doc: IXMLDocument);
  procedure AddNodeToTreeView(XMLNode: IXMLNode; ParentTreeNode: TTreeNode);
  var
    i: Integer;
    DisplayText: string;
    NewTreeNode: TTreeNode;
  begin
    if XMLNode = nil then
      Exit;

    DisplayText := XMLNode.NodeName;

    if XMLNode.HasAttribute('id') then
      DisplayText := DisplayText + ' (id=' + XMLNode.Attributes['id'] + ')';

    if (XMLNode.ChildNodes.Count = 0) and (Trim(XMLNode.Text) <> '') then
      DisplayText := DisplayText + ' = ' + Trim(XMLNode.Text);

    NewTreeNode := TreeView1.Items.AddChildObject(ParentTreeNode, DisplayText,
      TObject(XMLNode));
    NodeMap.Add(NewTreeNode, XMLNode);

    for i := 0 to XMLNode.ChildNodes.Count - 1 do
      AddNodeToTreeView(XMLNode.ChildNodes[i], NewTreeNode);
  end;

begin
  TreeView1.Items.BeginUpdate;
  try
    TreeView1.Items.Clear;

    if Assigned(Doc) and Assigned(Doc.DocumentElement) then
      AddNodeToTreeView(Doc.DocumentElement, nil);
  finally
    TreeView1.Items.EndUpdate;
    TreeView1.FullExpand;
  end;

end;

// file drag-drop
procedure TForm1.WMDropFiles(var Msg: TWMDropFiles);
var
  FilePath: array [0 .. MAX_PATH] of Char;
begin
  if DragQueryFile(Msg.Drop, 0, FilePath, MAX_PATH) > 0 then
  begin
    if FileExists(FilePath) and SameText(ExtractFileExt(FilePath), '.xml') then
    begin
      CurrentFileName := FilePath;
      LoadXMLFile(CurrentFileName); // <-- Use your existing file opening method
    end
    else
      ShowMessage('Please drop a valid .xml file.');
  end;

  DragFinish(Msg.Drop);
end;

procedure TForm1.PopulatePanel(TreeNode: TTreeNode);
var
  XMLNode: IXMLNode;
  i: Integer;
  HasText: Boolean;
begin
  // Reset UI
  StringGridAttributes.RowCount := 1;
  StringGridAttributes.Cells[0, 0] := 'Attribute';
  StringGridAttributes.Cells[1, 0] := 'Value';
  MemoNodeText.Clear;
  ListBoxChildren.Clear;

  // Hide everything and reset
  GroupBoxAttributes.Visible := False;
  GroupBoxText.Visible := False;
  GroupBoxChildren.Visible := False;

  LabelSelectNode.Visible := True;
  IsNodeSelected := False;
  HasNodeChildren := False;

  // Early exit if no node
  if not Assigned(TreeNode) then
    Exit;
  if not NodeMap.TryGetValue(TreeNode, XMLNode) then
    Exit;

  // Node is selected, update UI accordingly
  IsNodeSelected := True;
  LabelSelectNode.Visible := False;

  // --- Attributes ---
  if XMLNode.AttributeNodes.Count > 0 then
  begin
    GroupBoxAttributes.Visible := True;
    GroupBoxAttributes.Enabled := True;
    StringGridAttributes.RowCount := XMLNode.AttributeNodes.Count + 1;
    for i := 0 to XMLNode.AttributeNodes.Count - 1 do
    begin
      StringGridAttributes.Cells[0, i + 1] := XMLNode.AttributeNodes[i].NodeName;
      StringGridAttributes.Cells[1, i + 1] := XMLNode.AttributeNodes[i].Text;
    end;
  end
  else
  begin
    GroupBoxAttributes.Visible := True;  // Always visible when node selected
    GroupBoxAttributes.Enabled := False; // Disabled if no attributes
  end;

  // --- Text vs Children ---
  try
    HasText := Trim(XMLNode.Text) <> '';
  except
    HasText := False;
  end;
  HasNodeChildren := XMLNode.HasChildNodes;

  GroupBoxText.Visible := True;
  GroupBoxChildren.Visible := True;

  if HasNodeChildren then
  begin
    GroupBoxChildren.Enabled := True;
    GroupBoxText.Enabled := False;
    for i := 0 to XMLNode.ChildNodes.Count - 1 do
      ListBoxChildren.Items.Add(XMLNode.ChildNodes[i].NodeName);
    MemoNodeText.Clear;
  end
  else if HasText then
  begin
    GroupBoxText.Enabled := True;
    GroupBoxChildren.Enabled := False;
    MemoNodeText.Text := Trim(XMLNode.Text);
    ListBoxChildren.Clear;
  end
  else
  begin
    // No children and no text - enable both for user to add
    GroupBoxText.Enabled := True;
    GroupBoxChildren.Enabled := True;
    MemoNodeText.Clear;
    ListBoxChildren.Clear;
  end;
end;



procedure TForm1.ToggleRawView1Click(Sender: TObject);
begin
  ShowRaw := not ShowRaw;

  Memo1.Visible := ShowRaw;
  PanelDetailView.Visible := not ShowRaw;

  if ShowRaw and Assigned(FXMLDoc) then
  begin
    Memo1.Text := FXMLDoc.XML.Text; // 👈 Restore raw XML
  end;

  if not ShowRaw and Assigned(TreeView1.Selected) then
  begin
    PopulatePanel(TreeView1.Selected);
  end
  else if not ShowRaw then PopulatePanel(nil);
       
end;


procedure TForm1.TreeView1Click(Sender: TObject);
var
  Node: TTreeNode;
begin
  Node := TreeView1.GetNodeAt(TreeView1.ScreenToClient(Mouse.CursorPos).X,
    TreeView1.ScreenToClient(Mouse.CursorPos).Y);
  if Assigned(Node) and (Node = TreeView1.Selected) then
  begin
    PopulatePanel(Node);
  end;
end;

procedure TForm1.ClearPanel;
begin
  StringGridAttributes.RowCount := 1;
  StringGridAttributes.Cells[0, 0] := 'Attribute';
  StringGridAttributes.Cells[1, 0] := 'Value';
  StringGridAttributes.Visible := False;

  GroupBoxAttributes.Visible := False;
  GroupBoxText.Visible := False;
  GroupBoxChildren.Visible := False;

  MemoNodeText.Clear;
  ListBoxChildren.Clear;

  LabelSelectNode.Visible := True;
  IsNodeSelected := False;
end;


end.
