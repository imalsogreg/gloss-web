<!DOCTYPE html>
<html>
    <head>
        <link rel="stylesheet" href="codemirror.css">
        <link rel="stylesheet" href="theme/glossweb.css">
        <style type="text/css">
            html { height: 100% }
            body { position: absolute;
                   left: 2px;
                   right: 2px;
                   top: 2px;
                   bottom: 2px;
                   margin: 0 0 0 0 }
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
        <script type="text/javascript" src="codemirror-compressed.js"></script>
        <defaults/>
        <script type="text/javascript">
            var editor;

            window.onload = function() {
                editor = CodeMirror.fromTextArea(
                    document.getElementById("editor"), {
                        matchBrackets: true,
                        indentWithTabs: false,
                        indentUnit: 4,
                        tabMode: "shift",
                        lineNumbers: true,
                        theme: "glossweb",
                        onCursorActivity: function() {
                            editor.setLineClass(hlLine, null);
                            hlLine = editor.setLineClass(editor.getCursor().line, "activeline");
                            }
                        }
                    );

                var hlLine = editor.setLineClass(0, "activeline");

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
    <body>
        <div style="height: 70px">
            <div style="float:right">
                <a href="anim"><img src="animate.png" border=0 width=64 height=64 alt="Animate"></a>
                <a href="draw"><img src="picture.png" border=0 width=64 height=64 alt="Picture"></a>
                <a href="game"><img src="game.png" border=0 width=64 height=64 alt="Game"></a>
                <a href="sim" ><img src="simulate.png" border=0 width=64 height=64 alt="Simulate"></a>
            </div>
            <div>
                <intro/>
                Remember to import the <code>Graphics.Gloss</code> module.
                <a href="http://hackage.haskell.org/package/gloss/">
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
            </form>
        </div>
        <div style="position: fixed; bottom: 20px; right: 20px">
            <input type="button" value="Run"  onclick="run()"/>
            <input type="button" value="Stop" onclick="stop()"/>
        </div>
    </body>
</html>

