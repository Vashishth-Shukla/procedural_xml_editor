unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Menus,
  Xml.XMLDoc, Xml.XMLIntf, Vcl.ExtCtrls;

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
    procedure New1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure SaveAs1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

    procedure LoadXMLIntoTree(const XMLText: string);

  private
    CurrentFileName: string;
    Modified: Boolean;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);

var
  dlg: TForm;
begin
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
        end;
      mrNo: CanClose := True;
      mrCancel: CanClose := False;
    end;
    dlg.Free;
  end
  else
    CanClose := True;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  New1Click(nil); // simulate clicking File > New on startup
end;

procedure TForm1.Memo1Change(Sender: TObject);
begin
  Modified := True;


  // Optional live update — careful with performance!
  try
    LoadXMLIntoTree(Memo1.Text);
  except
    // silently fail if bad XML during typing
  end;

end;

procedure TForm1.New1Click(Sender: TObject);
begin
  CurrentFileName := '';

  Memo1.Clear;
  Memo1.Text := '<?xml version="1.0" encoding="UTF-8"?>' + sLineBreak +
              '<root>' + sLineBreak +
              '</root>';

  TreeView1.Items.Clear;
  TreeView1.Items.Add(nil, 'root');
  Modified := False;

  Caption := 'XML Editor - New File';
end;

procedure TForm1.Open1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    Memo1.Lines.LoadFromFile(OpenDialog1.FileName);
    CurrentFileName := OpenDialog1.FileName;
    TreeView1.Items.Clear;
    LoadXMLIntoTree(Memo1.Text);
    Modified := False;
    Caption := 'XML Editor - ' + OpenDialog1.FileName;
  end;
end;

procedure TForm1.Save1Click(Sender: TObject);
begin
  if CurrentFileName = '' then
  begin
    if SaveDialog1.Execute then
    begin
      CurrentFileName := SaveDialog1.FileName;
    end
    else
      Exit; // Cancelled

  end;

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


procedure TForm1.LoadXMLIntoTree(const XMLText: string);
var
  XMLDoc: IXMLDocument;

  procedure AddNodeToTreeView(XMLNode: IXMLNode; TreeNode: TTreeNode);
  var
    i: Integer;
    DisplayText: string;
    NewTreeNode: TTreeNode;
  begin
    if XMLNode = nil then Exit;

    DisplayText := XMLNode.NodeName;

    if XMLNode.HasAttribute('id') then
      DisplayText := DisplayText + ' (id=' + XMLNode.Attributes['id'] + ')';

    if (XMLNode.ChildNodes.Count = 0) and (Trim(XMLNode.Text) <> '') then
      DisplayText := DisplayText + ' = ' + Trim(XMLNode.Text);

    NewTreeNode := TreeView1.Items.AddChild(TreeNode, DisplayText);

    for i := 0 to XMLNode.ChildNodes.Count - 1 do
      AddNodeToTreeView(XMLNode.ChildNodes[i], NewTreeNode);
  end;

begin
  // Parse XML *first* before touching TreeView
  XMLDoc := TXMLDocument.Create(nil);
  XMLDoc.Options := [doNodeAutoCreate, doNodeAutoIndent];

  try
    XMLDoc.LoadFromXML(XMLText);
    XMLDoc.Active := True;

    if XMLDoc.DocumentElement = nil then Exit;

    // Only clear and update tree if XML is valid
    TreeView1.Items.BeginUpdate;
    try
      TreeView1.Items.Clear;
      AddNodeToTreeView(XMLDoc.DocumentElement, nil);
      TreeView1.FullExpand;
    finally
      TreeView1.Items.EndUpdate;
    end;
  except
    // Let calling method handle (or ignore) this
    raise;
  end;
end;

end.
