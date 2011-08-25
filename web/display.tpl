<!DOCTYPE html>
<html>
    <head>
        <title>Drawing</title>
        <!--[if IE]>
        <script type="text/javascript" src="excanvas.js"></script>
        <![endif]-->
        <script type="text/javascript" src="draw.js"></script>
        <displayScript/>
        <style type="text/css">
        body {
            margin: 0 0 0 0;
            text-align: center;
            }
        </style>
    </head>
    <body onload="displayInCanvas(document.getElementById('screen'), picture);">
        <canvas id="screen" width="500" height="500" style="border:solid black 1px"></canvas>
    </body>
</html>

