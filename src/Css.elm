module Css exposing (..)


cssStyle = """

body {
  padding: 0 10px;
}

table {
  margin: 20px 0;
  border: 1px solid #ddd;
  border-spacing: 1px;
}

td {
  background-color: #222;
  width: 10px;
  height: 10px;
  min-width: 10px;
  min-height: 10px;
  padding: 0;
}

table > tbody > tr > td {
  border: 1px solid #555;
}

td.alive {
  background-color: green;
}
"""

