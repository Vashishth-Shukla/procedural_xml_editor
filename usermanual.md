
# Delphi XML Editor - User Manual

## Introduction

This user manual explains how to use the Delphi XML Editor, a tool for creating, editing, and saving XML documents with a graphical user interface and optional raw text view.

---

## Getting Started

### Launching the Application

* Run the compiled executable.
* On launch, a new empty XML document is created by default.

### Creating a New Document

* Click **File > New**.
* A minimal XML document with a root node is created in a temporary location.
* You can begin editing immediately.

### Opening an Existing XML File

* Click **File > Open**.
* Select a `.xml` file using the file dialog.
* The file is parsed and loaded into both the tree view and memo view.

### Saving Changes

* Click **File > Save** to save to the current file.
* Click **File > Save As** to save under a new file name.
* Unsaved changes trigger a prompt on exit.

---

## Editing XML

### Using the Raw View

* The memo on the left side shows the raw XML content.
* Direct edits are allowed.
* If valid XML is entered, the tree view updates automatically.

### Using the Structured View

* Toggle with **View > Toggle Raw View**.
* Select a node in the tree to view/edit its:

  * Attributes (key-value pairs)
  * Text content (if no children)
  * Child nodes (listed by name)
* Use buttons to add/edit/delete attributes or children.

### Drag and Drop

* Drag any `.xml` file onto the app window.
* It will automatically load the file if valid.

---

## Interface Elements

* **Tree View**: Displays nodes hierarchically.
* **Memo**: Shows raw XML text.
* **Attribute Grid**: Editable grid of node attributes.
* **Node Text Box**: Edit text content of a node.
* **Child List Box**: Shows direct children of a node.
* **Panel Buttons**: Allow adding/editing/deleting node parts.

---

## Closing the Application

* Use **File > Exit**.
* If unsaved changes exist, you will be prompted to save.
* Temporary files are deleted upon closing if not saved.

---

## Troubleshooting

* **Invalid XML in memo**: The tree view will not update.
* **Tree not showing**: Ensure valid XML is loaded.
* **Can't save**: Verify you have permission to write to the selected path.

---

## FAQ

**Q: Can I undo changes?**

* Not yet. You can re-open the file to revert.

**Q: Can I export to JSON?**

* This feature was removed due to structural issues with mixed content.

**Q: Does it support attributes with namespaces?**

* No, namespace handling is not implemented.

---

## Contact

If you have suggestions, improvements, or bugs to report, contact the maintainer or open an issue in the repository.
