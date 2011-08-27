<!DOCTYPE html>
<html>
    <head>
        <title>Drawing</title>
        <!--[if IE]>
        <script type="text/javascript" src="excanvas.js"></script>
        <![endif]-->
        <script type="text/javascript" src="draw.js"></script>
        <displayScript/>
        <link rel="stylesheet" type="text/css" href="tooltip.css">
        <style type="text/css">
        body {
            margin: 0 0 0 0;
            text-align: center;
            }
        </style>
        <script type="text/javascript">
            function init()
            {
                var canvas = document.getElementById("screen");

                if (window.picture)
                {
                    displayInCanvas(canvas, picture);
                }

                if (window.eventURI)
                {
                    var eventSource = new EventSource(eventURI);
                    eventSource.onmessage = function(event) {
                        var pic = JSON.parse(event.data);
                        displayInCanvas(canvas, pic);
                    }
                }
            }
        </script>
    </head>
    <body onload="init()" style="overflow:hidden">
        <canvas id="screen"
                width="500"
                height="500"
                style="border:solid black 1px"
                onmouseover="tooltip.show();"
                onmouseout="tooltip.hide();"
                onclick="tooltip.pin();">
        </canvas>
        <script type="text/javascript" src="tooltip.js"></script>
    </body>
</html>

