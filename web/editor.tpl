<!DOCTYPE html>
<html>
    <head>
        <script type="text/javascript" src="codemirror-compressed.js"></script>
        <link rel="stylesheet" href="codemirror.css">
        <link rel="stylesheet" href="theme/default.css">
        <style type="text/css">
            html { height: 100% }
            body { min-height: 98% }
            .CodeMirror {
                border: dotted gray 1px;
                height: 100%;
                overflow: auto;
                max-height: 100%;
                }
            .CodeMirror-scroll {
                height: 100%;
                max-height: 100%;
                }
        </style>
        <defaults/>
        <script type="text/javascript">
            var editor;

            window.onload = function() {
                editor = CodeMirror.fromTextArea(
                    document.getElementById("editor"), {
                        matchBrackets: true,
                        indentWithTabs: false,
                        identUnit: 4,
                        tabMode: "indent",
                        lineNumbers: true
                        }
                    );

                var all = document.cookie;
                var list = all.split("; ");
                var found = false;
                for (var i = 0; i < list.length; i++)
                {
                    var cookie = list[i];
                    var p = cookie.indexOf("=");
                    var name = cookie.substring(0,p);
                    if (name == sourceCookie)
                    {
                        var value = decodeURIComponent(cookie.substring(p+1));
                        editor.setValue(value);
                        found = true;
                        break;
                    }
                }

                if (!found)
                {
                    editor.setValue(initialSource);
                }

                stop();
                editor.focus();
            };

            function run()
            {
                document.getElementById("editBox").style.right = '525px';
                document.getElementById("runBox" ).style.display = 'block';
                var s = editor.getValue();
                document.cookie = sourceCookie + "=" + encodeURIComponent(s)
                    + "; max-age = " + (1000 * 365 * 24 * 60 * 60);
                document.getElementById("form").submit();
                editor.focus();
            }
            
            function stop()
            {
                document.getElementById("runBox" ).style.display = 'none';
                document.getElementById("editBox").style.right = '0px';
                window.display.location.href = "about:blank";
            }
        </script>
    </head>
    <body style="position: relative">
        <div style="height: 70px">
            <div style="float:right">
                <a href="anim"><img src="animate.png" border=0 width=64 height=64 alt="Animate"></a>
                <a href="draw"><img src="picture.png" border=0 width=64 height=64 alt="Picture"></a>
                <a href="sim" ><img src="simulate.png" border=0 width=64 height=64 alt="Simulate"></a>
            </div>
            <div>
                <intro/>
                Remember to import the <code>Graphics.Gloss</code> module.
                <a href="http://hackage.haskell.org/packages/archive/gloss/latest/doc/html/Graphics-Gloss-Data-Picture.html">
                Check here for things you can do</a>.  Have fun!
            </div>
            <hr style="clear:both">
        </div>
        <div style="position: absolute; top: 70px; bottom: 0px; left: 0px; right: 0px">
            <form id="form" action="$(action)" method="post" target="display" style="height: 100%">
            <div id="runBox" style="float: right">
                <iframe frameborder="0" name="display" src="about:blank" width="525" height="525"></iframe>
            </div>
            <div id="editBox" style="position: absolute; top: 0px; bottom: 0px; left: 0px; right: 525px">
                <textarea style="width: 100%; height: 100%" id="editor" name="source">Loading...</textarea>
            </div>
            <div style="position: absolute; height: 30px; bottom: 0px; left: 0px; right: 0px; text-align: right">
                <input type="button" value="Run" onclick="run()"/>
                <input type="button" value="Stop" onclick="stop()"/>
            </div>
            </form>
        </div>
    </body>
</html>

