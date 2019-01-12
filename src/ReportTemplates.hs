

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-----------------------------------------------------------------------------
--
-- Module    :  TestData
-- Copyright   :
-- License   :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module ReportTemplates  where

import           Data.Text
import           Text.RawString.QQ

{-# ANN module ("HLint: ignore unused LANGUAGE pragma":: Prelude.String) #-}

summaryTemplate :: Text
summaryTemplate = [r|
<!DOCTYPE html>
<html lang="en">


<head>
  <meta charset="UTF-8">
  <title>#header</title>

  <title>Pie Chart with Custom Tooltips</title>
  <script src=".././ChartBundle.js"></script>
  <script src=".././utils.js"></script>

  <script>
    Chart.defaults.global.tooltips.custom = function(tooltip) {
      // Tooltip Element
      var tooltipEl = document.getElementById('chartjs-tooltip');

      // Hide if no tooltip
      if (tooltip.opacity === 0) {
        tooltipEl.style.opacity = 0;
        return;
      }

      // Set caret Position
      tooltipEl.classList.remove('above', 'below', 'no-transform');
      if (tooltip.yAlign) {
        tooltipEl.classList.add(tooltip.yAlign);
      } else {
        tooltipEl.classList.add('no-transform');
      }

      function getBody(bodyItem) {
        return bodyItem.lines;
      }

      // Set Text
      if (tooltip.body) {
        var titleLines = tooltip.title || [];
        var bodyLines = tooltip.body.map(getBody);

        var innerHtml = '<thead>';

        titleLines.forEach(function(title) {
          innerHtml += '<tr><th>' + title + '</th></tr>';
        });
        innerHtml += '</thead><tbody>';

        bodyLines.forEach(function(body, i) {
          var colors = tooltip.labelColors[i];
          var style = 'background:' + colors.backgroundColor;
          style += '; border-color:' + colors.borderColor;
          style += '; border-width: 2px';
          var span = '<span class="chartjs-tooltip-key" style="' + style + '"></span>';
          innerHtml += '<tr><td>' + span + body + '</td></tr>';
        });
        innerHtml += '</tbody>';

        var tableRoot = tooltipEl.querySelector('table');
        tableRoot.innerHTML = innerHtml;
      }

      var positionY = this._chart.canvas.offsetTop;
      var positionX = this._chart.canvas.offsetLeft;

      // Display, position, and set styles for font
      tooltipEl.style.opacity = 1;
      tooltipEl.style.left = positionX + tooltip.caretX + 'px';
      tooltipEl.style.top = positionY + tooltip.caretY + 'px';
      tooltipEl.style.fontFamily = tooltip._fontFamily;
      tooltipEl.style.fontSize = tooltip.fontSize;
      tooltipEl.style.fontStyle = tooltip._fontStyle;
      tooltipEl.style.padding = tooltip.yPadding + 'px ' + tooltip.xPadding + 'px';
    };

    var config = {
      type: 'pie',
      data: {
        datasets: [{
          data: [#passCount, #failedCount, #queriesCount, #skippedCount, #deferredCount, #pendingCount],
          backgroundColor: [
            window.chartColors.green,
            window.chartColors.red,
            window.chartColors.yellow,
            "#a2a5a2",
            "#cfd3cf",
            "#e3e5e3"
          ],
        }],
        labels: [
          "Passed",
          "Failed",
          "Queries",
          "Skipped",
          "Deferred",
          "Pending",
        ]
      },
      options: {
        responsive: true,
        legend: {
          display: true,
          position: 'right',
          labels: {
            fontColor: "White"
          }
        },
        tooltips: {
          enabled: true
        }
      }
    };

    window.onload = function() {
      var ctx = document.getElementById("chart-area").getContext("2d");
      window.myPie = new Chart(ctx, config);
    };
  </script>



  <link rel="stylesheet" href=".././css/style.css">


</head>

<body>


  <section id="wrapper">
    <header>#header</header>
    <left>

      <section id="resultSummary">
        <sumHeader>
            Test Results
        </sumHeader>

        <pie>
          <div id="canvas-holder">
            <canvas id="chart-area"></canvas>
            <div id="chartjs-tooltip">
            </div>
          </div>
        </pie>

        <resultgrid>

            <table id="resultsTbl">
              <thead>
              <tr>
                <th>Status</th>
                <th>Count</th>
                <th>Status</th>
                <th>Count</th>
              </tr>
              <tr>
                <td>Pass</td>
                <td>#passCount</td>
                <td>Skipped</td>
                <td>#skippedCount</td>
              </tr>
              <tr>
                <td>Failed</td>
                <td>#failedCount</td>
                <td>Deferred</td>
                <td>#deferredCount</td>
              </tr>
              <tr>
                <td>Queries</td>
                <td>#queriesCount</td>
                <td>Pending</td>
                <td>#pendingCount</td>
              </tr>
           </thead>
            </table>

        </resultgrid>

      </section>
    </left>
    <right>

      <div class="recordsbox">
        <div class="subtitle">
        Failures
        </div>
        <!--#Failures-->
        <!--
        <div class="subtitle">No Failures in Test Run</div>
        <ol id="failList" class="recordsList">
         <li>#failItem</li>
        </ol>
        -->
      </div>

      <div id= "queriesbox" class="recordsbox">
        <div class="subtitle">
        Queries
      </div>
      <!--#Queries-->
      <!--
      <div class="subtitle"><No Queries in Test Run></div>
      <ol id="queryList" class="recordsList">
      <li>#queryItem</li>
      </ol>
      -->
      </div>
    </right>
    <footer>#footer</footer>
  </section>


</body>

</html>
|]
