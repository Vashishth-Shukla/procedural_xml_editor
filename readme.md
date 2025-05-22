# Delphi XML Editor

## Overview

This is a lightweight, interactive XML editor built with Delphi. It enables users to:

* Create, open, and save XML documents
* View XML structure in a `TTreeView`
* Edit raw XML directly in a `TMemo`
* View and edit attributes, node text, and child elements via an interactive panel
* Drag and drop XML files into the editor

This tool is intended for quick editing and exploration of XML documents with a simple UI.

---

## Features

* **File Menu**:

  * `New`: Creates a new temporary XML document with a default root.
  * `Open`: Opens existing XML files.
  * `Save`: Saves changes to the current file.
  * `Save As`: Save to a new location.
  * `Exit`: Exits the application, prompting to save unsaved changes.

* **View**:

  * Toggle between raw XML text view and a structured visual editor with node/attribute editing.

* **Drag & Drop**:

  * Drop `.xml` files directly onto the application window to open them.

* **Tree View**:

  * Displays the hierarchy of the XML document.
  * Selecting a node shows its attributes, child nodes, or inner text.

* **Attribute & Text Panel**:

  * Edit node attributes and text using provided controls.

---

## Technical Details

* Language: **Delphi** (tested on Delphi 12)
* Uses Delphi's built-in `IXMLDocument` and `TXMLDocument` for parsing and managing XML
* UI elements: `TTreeView`, `TMemo`, `TStringGrid`, `TListBox`, `TButton`, `TPanel`
* Utilizes `System.JSON`, `System.IOUtils`, and `System.Generics.Collections`
* Safe handling of unsaved changes and temporary files

---

## Usage

1. Launch the application.
2. Use the **File > New** option to start a new XML document.
3. Use **Open** to load an existing `.xml` file.
4. Edit directly in the memo (raw view) or use the structured view to update node text, attributes, or children.
5. Changes to the memo immediately affect the tree view if valid XML is entered.
6. Use **Save** or **Save As** to write changes back to disk.

---

## Limitations

* No XML schema (XSD) validation
* No support for namespaces or mixed content nodes
* Tree selection does not highlight corresponding text in raw memo
* No undo/redo support
* No JSON export (disabled/removed)

---

## Planned Enhancements

* XSD validation support
* Line number gutter for memo view
* Better synchronization between tree and memo
* JSON export (structured)
* Enhanced UI for editing nodes (modal dialogs, inline editing)

---

## License

Provided for educational and internal tooling use. No formal license applied.
