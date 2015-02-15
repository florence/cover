window.onload = function() {

  var extractDataFromRow = function(tr, className) {
    var td = tr.querySelector('td.' + className);
    var a = td.querySelector('a');
    if (a) {
      return a.innerHTML;
    } else {
      return td.innerHTML;
    }
  };

  var removeRows = function(table, rows) {
    for (var i = 0; i < rows.length; i++) {
      table.removeChild(rows[i]);
    }
  };

  var sortTable = function(className, compare) {
    var table = document.querySelector('table.file-list tbody');
    var rows = Array.prototype.slice.call(table.querySelectorAll('tr'), 0);
    removeRows(table, rows);

    rows.sort(function(a, b) {
      var aActual = extractDataFromRow(a, className);
      var bActual = extractDataFromRow(b, className);
      return compare(aActual, bActual);
    });
    for (var i = 0; i < rows.length; i++) {
      rows[i].className = 'file-info' + (((i % 2) == 0) ? ' stripe' : '');
      table.appendChild(rows[i]);
    }
  };

  var createTableSorter = function(className, compare) {
    return function() {
      sortTable(className, compare);
    };
  };

  var createSorter = function(className, sortAscending, sortDescending) {
    var ascending = document.querySelector('th.' + className + ' .sorter div.sort-icon-up');
    var descending = document.querySelector('th.' + className + ' .sorter div.sort-icon-down');
    ascending.addEventListener('click', createTableSorter(className, sortAscending));
    descending.addEventListener('click', createTableSorter(className, sortDescending));
  };

  var stringCompareAsc = function(a, b) { return a.localeCompare(b); };
  var stringCompareDesc = function(a, b) { return b.localeCompare(a); };
  var floatCompareAsc = function(a, b) { return parseFloat(a) - parseFloat(b); };
  var floatCompareDesc = function(a, b) { return parseFloat(b) - parseFloat(a); };
  createSorter('file-name', stringCompareAsc, stringCompareDesc);
  createSorter('coverage-percentage', floatCompareAsc, floatCompareDesc);
  createSorter('covered-expressions', floatCompareAsc, floatCompareDesc);
  createSorter('total-expressions', floatCompareAsc, floatCompareDesc);
  sortTable('file-name', stringCompareAsc);
};
