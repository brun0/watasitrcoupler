import Plotly from 'plotly.js-dist';
import $ from 'jquery';
import 'jquery-csv';

var csvData;

function loadFile(data) {
    csvData = $.csv.toObjects(data);
    // build selects
    $('#f1').html('');
    $('#f2').html('');
    $('#f3').html('');
    $('#twod-field1').html('');
    $('#twod-field2').html('');
    $('#twod-condition-field').html('');
    var keys = Object.keys(csvData[0]);
    for (var i = 0; i < keys.length; i++) {
        $('#f1').append(
            $('<option/>', {value: keys[i]}).text(keys[i])
        );
        $('#f2').append(
            $('<option/>', {value: keys[i]}).text(keys[i])
        );
        $('#f3').append(
            $('<option/>', {value: keys[i]}).text(keys[i])
        );
        $('#twod-field1').append(
            $('<option/>', {value: keys[i]}).text(keys[i])
        );
        $('#twod-field2').append(
            $('<option/>', {value: keys[i]}).text(keys[i])
        );
        $('#twod-condition-field').append(
            $('<option/>', {value: keys[i]}).text(keys[i])
        );
    }
    $('#f1 option:eq(0)').attr('selected', 'selected');
    $('#f2 option:eq(1)').attr('selected', 'selected');
    $('#f3 option:eq(2)').attr('selected', 'selected');
    $('#twod-field1 option:eq(1)').attr('selected', 'selected');
    $('#twod-field2 option:eq(2)').attr('selected', 'selected');
    $('#twod-condition-field option:eq(0)').attr('selected', 'selected');

    $('#plots').show();
    redraw3d();
    redraw2d();
    $('body, #3d_plot, #2d_plot').css('cursor', 'default');
}

// get all values for one column
function unpack(rows, key) {
	return rows.map(function(row) {
        return row[key];
    });
}

// get values of one column with a condition on another column
function unpackWhere(rows, key, selectKey, selectValue) {
    var selectValueString = selectValue.toString();
    var res = [];
    var row;
    for (var i = 0; i < rows.length; i++) {
        row = rows[i];
        if (row[selectKey].toString() === selectValueString) {
            res.push(row[key]);
        }
    }
    return res;
}

function getUniqueValuesOfColumn(rows, colName) {
    return [...new Set(unpack(rows, colName))];
}

function display3d(x_axis, y_axis, z_axis) {
    var trace1 = {
        x: unpack(csvData, x_axis), y: unpack(csvData, y_axis), z: unpack(csvData, z_axis),
        mode: 'markers',
        name: 'plpl',
        marker: {
            size: 3,
            color: '#FF3000',
            opacity: 0.8
        },
        type: 'scatter3d'
    };

    var data = [trace1];
    var layout = {
        margin: {
            l: 0,
            r: 0,
            b: 50,
            t: 50
        },
        title: $('#csv_file').val().split('\\').pop(),
        scene: {
            xaxis:{title: x_axis},
            yaxis:{title: y_axis},
            zaxis:{title: z_axis},
        },
    };
    Plotly.newPlot('3d_plot', data, layout);
}

// select rows with a condition, then take 2 selected axis (columns)
function display2d(x_axis, y_axis, selectValueString, selectColumn) {
    var trace;
    var selectValueList = [];
    if (selectValueString === '*') {
        selectValueList = getUniqueValuesOfColumn(csvData, selectColumn);
    } else {
        var comaSpl = selectValueString.split(',');
        var pattDash = new RegExp('-');
        var comaPart;
        var dash1, dash2;
        for (var i = 0; i < comaSpl.length; i++) {
            comaPart = comaSpl[i];
            if (pattDash.test(comaPart)) {
                dash1 = parseInt(comaPart.split('-')[0]);
                dash2 = parseInt(comaPart.split('-')[1]);
                for (var j = dash1; j <= dash2; j++) {
                    selectValueList.push(j);
                }
            } else {
                selectValueList.push(parseInt(comaPart));
            }
        }
    }
    //console.log('selected values for column '+selectColumn);
    //console.log(selectValueList);

    var data = [];
    var selectValue;
    var xValues, yValues;
    // for each selected row, get what we want and make one trace from it
    for (var i = 0; i < selectValueList.length; i++) {
        selectValue = selectValueList[i];
        xValues = unpackWhere(csvData, x_axis, selectColumn, selectValue);
        yValues = unpackWhere(csvData, y_axis, selectColumn, selectValue);
        // avoid values with no data
        if (xValues.length > 0 && xValues.length === yValues.length) {
            trace = {
                x: xValues, y: yValues,
                mode: 'line',
                name: selectColumn + '=' + selectValue,
                type: 'scatter'
            };
            data.push(trace);
        }
    }

    var layout = {
        title: y_axis+'/'+x_axis+' for '+selectColumn+'='+selectValueList,
        xaxis: {
            title: x_axis,
            showgrid: true,
            zeroline: true
        },
        yaxis: {
            title: y_axis,
            showgrid: true,
            zeroline: true
        },
    };
    Plotly.newPlot('2d_plot', data, layout);
}

function redraw3d() {
    var f1 = $('#f1').val();
    var f2 = $('#f2').val();
    var f3 = $('#f3').val();
    display3d(f1, f2, f3);
}
function redraw2d() {
    var twodF1 = $('#twod-field1').val();
    var twodF2 = $('#twod-field2').val();
    var twodSelectField = $('#twod-condition-field').val();
    var ids = $('#entity-id').val();
    display2d(twodF1, twodF2, ids, twodSelectField);
}

$('#f1, #f2, #f3').change(function() {
    redraw3d();
});

$('#twod-field1, #twod-field2, #twod-condition-field').change(function() {
    redraw2d();
});
$('#entity-id').on('keypress', function(e) {
    if (e.key === 'Enter') {
        redraw2d();
    }
});

$('#csv_file').val('');
$('#csv_file').change(function(e) {
    $('body, #3d_plot, #2d_plot').css('cursor', 'wait');
    var file = e.target.files[0];
    var reader = new FileReader();
    reader.onload = function () {
        var data = reader.result;
        loadFile(data);
    };
    reader.readAsText(file);
});

$('#main').show();
$('#plots').hide();