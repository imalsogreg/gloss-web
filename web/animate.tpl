<!DOCTYPE html>
<html>
    <head>
        <title>Animation</title>
        <style type="text/css">
        body {
            margin: 0 0 0 0;
            text-align: center;
            }
        </style>
        <!--[if IE]>
        <script type="text/javascript" src="excanvas.js"></script>
        <![endif]-->
        <script type="text/javascript" src="draw.js"></script>
        <streamScript/>
        <script type="text/javascript">
        function startAnim()
        {
            var canvas = document.getElementById("screen");
            var eventSource = new EventSource(eventURI);
            eventSource.onmessage = function(event) {
                var pic = JSON.parse(event.data);
                displayInCanvas(canvas, pic);
            }
        }
        </script>
    </head>
    <body onload="startAnim();">
        <canvas id="screen" width="500" height="500" style="border:solid black 1px"></canvas>
    </body>
</html>

