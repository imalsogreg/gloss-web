<html>
    <head>
        <script type="text/javascript" src="codemirror-compressed.js"></script>
        <link rel="stylesheet" href="codemirror.css">
        <link rel="stylesheet" href="theme/default.css">
        <style type="text/css">
            .CodeMirror {
                border: solid gray 1px;
                width: 100%;
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
    </head>
    <body>
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
        <form id="form" action="$(action)" method="post" target="display">
        <table border="0" width="100%" height="80%">
        <tr><td>
            <textarea style="width: 100%; height: 100%" id="editor" name="source">Loading...</textarea>
        </td><td width="525">
            <iframe frameborder="0" name="display" src="about:blank" style="width: 100%; height: 100%"></iframe>
        </td></tr></table>

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

                editor.focus();
            };

            function run()
            {
                var s = editor.getValue();
                document.cookie = sourceCookie + "=" + encodeURIComponent(s)
                    + "; max-age = " + (1000 * 365 * 24 * 60 * 60);
                document.getElementById("form").submit();
                editor.focus();
            }
            
            function stop()
            {
                window.display.location.href = "about:blank";
            }
        </script>

        <div style="clear: both; text-align: center">
          <input type="button" value="Run" onclick="run()"/>
          <input type="button" value="Stop" onclick="stop()"/>
        </div>
        </form>
    </body>
</html>

