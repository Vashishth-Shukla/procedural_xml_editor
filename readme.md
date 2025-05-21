# Delphi XML Editor

## Overview

This project is a simple interactive XML editor built in Delphi. It allows users to:

- Create new XML files
- Open and display existing XML files
- Edit XML content in a text memo
- Visualize the XML structure in a tree view
- Save changes back to XML files

The editor includes basic XML parsing and tree-building functionality, providing a foundational tool for XML editing and exploration.

---

## Features

- File menu with options: New, Open, Save, Save As, Exit
- Displays XML structure in a `TTreeView`
- Raw XML text editing in a `TMemo`
- Synchronized selection: selecting a node in the tree highlights corresponding XML in the memo (basic)
- Validation checks when loading XML to avoid crashes on invalid input
- Adjustable layout with a splitter (optional enhancement)

---

## Technical Details

- Developed in Delphi (tested with Delphi 10.4 Sydney and newer)
- Uses Delphi’s built-in `TXMLDocument` for XML parsing
- Exception-safe XML loading and parsing
- Supports UTF-8 encoded XML files

---

## Setup and Usage

1. Open the project in Delphi.
2. Build and run the application.
3. Use the **File** menu to:
   - **New**: Start a new empty XML document.
   - **Open**: Load an existing XML file (UTF-8 encoded).
   - **Save/Save As**: Save changes to disk.
4. Edit XML text directly in the memo.
5. The tree view updates to reflect the XML structure when valid XML is present.
6. Select tree nodes to highlight corresponding tags in the memo (basic implementation).

---

## Known Limitations

- Live XML validation is basic and may fail during typing.
- Tree-to-text cursor mapping is approximate and may not handle repeated tags precisely.
- No built-in XML schema (XSD) validation yet.
- The text editor (`TMemo`) does not support line numbers natively.
- No undo/redo or advanced editing features.
- Limited error feedback on malformed XML.

---

## Future Enhancements

- Integrate XSD validation (using MSXML or other libraries)
- Improve tree-to-text synchronization with exact character offsets
- Add line numbers gutter next to the XML editor
- Implement undo/redo and better edit controls (`TRichEdit` or third-party editor)
- Support exporting to other formats (e.g., JSON)
- Add a customizable toolbar and keyboard shortcuts

---

## External Libraries

- Optionally integrate [OXML](http://www.kluug.net/oxml.php) for advanced XML handling.
- Consider MSXML COM interfaces for schema validation and line/column info.

---

## License

This project is provided as-is for learning and development purposes.

---

## Contact

For questions or contributions, please open an issue or contact the maintainer.