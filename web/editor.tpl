<html>
    <head>
        <script type="text/javascript" src="ace.js" charset="utf-8"></script>
        <script type="text/javascript" src="mode-haskell.js" charset="utf-8"></script>
        <defaults/>
    </head>
    <body>
        <div>
            <intro/>
            Remember to import the <code>Graphics.Gloss</code> module.
            <a href="http://hackage.haskell.org/packages/archive/gloss/latest/doc/html/Graphics-Gloss-Data-Picture.html">
            Check here for things you can do</a>.  Have fun!
            <hr>
        </div>
        <form id="form" action="$(action)" method="post" target="display">
        <div style="float:left; position: relative; width: 650px; height: 80%" id="editor">Loading...</div>
        <div style="float:right; valign:center">
            <iframe frameborder="0" name="display" src="about:blank" width=525 height="80%"></iframe>
        </div>
        <script type="text/javascript">
            var editor;

            function resizeEditor()
            {
                document.getElementById("editor").style.width
                    = (window.innerWidth - 575) + "px";
            }

            window.onload = function() {
                resizeEditor();

                editor = ace.edit("editor");

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
                        editor.getSession().setValue(value);
                        found = true;
                        break;
                    }
                }

                if (!found)
                {
                    editor.getSession().setValue(initialSource);
                }

                var HaskellMode = require('ace/mode/haskell').Mode;
                editor.getSession().setMode(new HaskellMode());
                editor.focus();
            };

            window.onresize = function() {
                resizeEditor();
            }

            function run()
            {
                var s = editor.getSession().getValue();
                document.cookie = sourceCookie + "=" + encodeURIComponent(s)
                    + "; max-age = " + (1000 * 365 * 24 * 60 * 60);
                document.getElementById("codefield").value = s
                document.getElementById("form").submit();
                editor.focus();
            }
            
            function stop()
            {
                window.display.location.href = "about:blank";
            }
        </script>

        <div style="clear: both; text-align: center">
          <input id="codefield" type="hidden" name="source" value=""/>
          <input type="button" value="Run" onclick="run()"/>
          <input type="button" value="Stop" onclick="stop()"/>
        </div>
        </form>
    </body>
</html>

