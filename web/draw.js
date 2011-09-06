/*
 * Tooltip function originally by Michael Leigeber, but modified here to
 * include pinning and to simplify in a few places.
 */
var tooltip = function() {
	var top = 3;
	var left = 0;
	var speed = 10;
	var timer = 20;
	var endalpha = 90;
	var alpha = 0;
	var tt = null;
	var c, h;

	return {
		show: function() {
			if(tt == null)
			{
				tt = document.createElement('div');
				tt.className = 'tt';
				c = document.createElement('div');
				c.className = 'ttcont';
				tt.appendChild(c);
				document.body.appendChild(tt);
				tt.style.opacity = 0;
				tt.style.filter = 'alpha(opacity=0)';
				document.onmousemove = this.pos;
			}

			tt.style.display = 'block';
			c.innerHTML = '()';
			tt.style.width = '90px';
			tt.style.whiteSpace = 'nowrap';
			h = parseInt(tt.offsetHeight) + top;
			clearInterval(tt.timer);
			tt.timer = setInterval(function() { tooltip.fade(1) }, timer);
		},

		pos: function(e) {
			if (tt == null)
			{
				tooltip.show();
				return;
			}

			var u,l;

			if (e.pageX)
			{
				var u = e.pageY;
				var l = e.pageX;
			}
			else
			{
				var u = event.clientY + document.documentElement.scrollTop;
				var l = event.clientX + document.documentElement.scrollLeft;
			}

			tt.style.top = (u - h) + 'px';
			tt.style.left = (l + left) + 'px';
			c.innerHTML = "(" + (l - 264) + "," + (252 - u) + ")";
		},

		fade: function(d) {
			if (tt == null) return;

			var a = alpha;
			if((a != endalpha && d == 1) || (a != 0 && d == -1))
			{
				var i = speed;
				if(endalpha - a < speed && d == 1)
				{
					i = endalpha - a;
				}
				else if(alpha < speed && d == -1)
				{
					i = a;
				}
				alpha = a + (i * d);
				tt.style.opacity = alpha * .01;
				tt.style.filter = 'alpha(opacity=' + alpha + ')';
			}
			else
			{
				clearInterval(tt.timer);
				if(d == -1){tt.style.display = 'none'}
			}
		},

		hide: function() {
			if (tt != null)
			{
				clearInterval(tt.timer);
				tt.timer = setInterval(function(){tooltip.fade(-1)},timer);
			}
		},

		pin: function() {
			if (tt == null || tt.style.display == 'none') return;

			var pinned = tt;
			tt = null;
			alpha = 0;

			var u,l;
			if (event.pageX)
			{
				u = event.pageY;
				l = event.pageX;
			}
			else
			{
				u = event.clientY + document.documentElement.scrollTop;
				l = event.clientX + document.documentElement.scrollLeft;
			}

			pinned.style.left = (l - 6) + 'px';
			pinned.style.top = (u - h + 1) + 'px';
			pinned.style.filter = 'alpha(opacity=' + endalpha + ')';

			pinned.onclick = function()
			{
				pinned.parentNode.removeChild(pinned);
			}
		}
	};
}();


/*
 * Stream class represents a binary stream from which various kinds of data can
 * be read.
 */
function Stream(b)
{
    var index = 0;

    this.popByte = function()
    {
        return b.charCodeAt(index++);
    }

    this.peekByte = function()
    {
        return b.charCodeAt(index);
    }

    this.popWord32 = function()
    {
        var a = this.popByte();
        var b = this.popByte();
        var c = this.popByte();
        var d = this.popByte();

        return (a << 24) | (b << 16) | (c << 8) | (d);
    }

    this.popFloat = function()
    {
        var i = this.popWord32();
        var sign = ((i >> 31) & 0x00000001) ? -1 : 1;
        var expt = (i >> 23) & 0x000000ff;
        var mant = (i      ) & 0x007fffff;

        if (expt == 0)
        {
            return sign * Math.pow(2, -126) * (mant / 0x800000);
        }
        else if (expt == 0xff)
        {
            if (mant != 0) return Number.NaN;
            else return sign * Infinity;
        }
        else
        {
            return sign * Math.pow(2, expt - 127) * (1 + mant / 0x800000);
        }
    }

    this.popBytes = function(len)
    {
        var i = index;
        index += len;
        return b.slice(i, i + len);
    }

    this.popUtf8 = function(len)
    {
        var utftext = this.popBytes(len);
    	var res = [];

    	var i = 0, j = 0;
    	var c = c1 = c2 = 0;

    	while ( i < utftext.length )
    	{
    		c = utftext.charCodeAt(i);

    		if (c < 128)
    		{
    			res[j++] = String.fromCharCode(c);
	    		i++;
	    	}
    		else if ((c > 191) && (c < 224))
    		{
    			c2 = utftext.charCodeAt(i+1);
	    		res[j++] = String.fromCharCode(((c & 31) << 6) | (c2 & 63));
	    		i += 2;
	    	}
    		else
    		{
    			c2 = utftext.charCodeAt(i+1);
	    		c3 = utftext.charCodeAt(i+2);
	    		res[j++] = String.fromCharCode(((c & 15) << 12) | ((c2 & 63) << 6) | (c3 & 63));
	    		i += 3;
	    	}
        }

    	return res.join("");
	}
}

/*
 * Base64 decoding, in case the browser doesn't implement it (looking at you, IE)
 */
if (!window.atob)
{
    window.atob = function(a)
    {
        var b64 = function(c)
        {
            if      (c >= 65 && c <  91) return c - 65;
            else if (c >= 97 && c < 123) return c - 71;
            else if (c >= 48 && c <  58) return c +  4;
            else if (c == 43           ) return 62;
            else if (c == 47           ) return 63;
            else return 0;
        }

        var result = [];

        var i;
        for (i = 0; i < a.length; i += 4)
        {
            var x = a.charCodeAt(i);
            var y = a.charCodeAt(i+1);
            var z = a.charCodeAt(i+2);
            var w = a.charCodeAt(i+3);

            if (z == 61)
            {
                var ix = b64(x);
                var iy = b64(y);
                result.push(String.fromCharCode(0xff & (ix << 2) | (iy >> 4)));
            }
            else if (w == 61)
            {
                var ix = b64(x);
                var iy = b64(y);
                var iz = b64(z);
                result.push(String.fromCharCode(0xff & (ix << 2) | (iy >> 4)),
                            String.fromCharCode(0xff & (iy << 4) | (iz >> 2)));
            }
            else
            {
                var ix = b64(x);
                var iy = b64(y);
                var iz = b64(z);
                var iw = b64(w);
                result.push(String.fromCharCode(0xff & (ix << 2) | (iy >> 4)),
                            String.fromCharCode(0xff & (iy << 4) | (iz >> 2)),
                            String.fromCharCode(0xff & (iz << 6) | (iw     )));
            }
        }

        return result.join("");
    }
}

/*
 * Cached drawing context.
 */
var ctx = null;

/*
 * Top-level function to draw a picture in a given canvas.
 */
function displayInCanvas(c, pic)
{
    var decoded = atob(pic);

    if (ctx == null) ctx = c.getContext("2d");
    ctx.clearRect(0,0,500,500);
    ctx.save();
    ctx.translate(250, 250);
    ctx.scale(1, -1);
    ctx.textAlign = "left";
    ctx.textBaseline = "alphabetic";
    ctx.lineWidth = 0;
    ctx.font = "100px Times Roman";
    display(ctx, new Stream(decoded), 1, 0, 0, 1, 0, 0);
    ctx.restore();
}

/*
 * The recursive drawing function.
 */
function display(ctx, p, a, b, c, d, e, f)
{
    var tag = p.popByte();

    if (tag == 1)
    {
        // Blank
    }
    else if (tag == 2)
    {
        // Polygon
        var len = p.popWord32();
        if (len > 0)
        {
            ctx.save();
            ctx.transform(a, b, c, d, e, f);
            ctx.beginPath();
            ctx.moveTo(p.popFloat(), p.popFloat());
            var i;
            for (i = 1; i < len; i++)
            {
                ctx.lineTo(p.popFloat(), p.popFloat());
            }
            ctx.restore();
            ctx.fill();
        }
    }
    else if (tag == 3)
    {
        // Line
        var len = p.popWord32();
        if (len > 0)
        {
            ctx.save();
            ctx.transform(a, b, c, d, e, f);
            ctx.beginPath();
            ctx.moveTo(p.popFloat(), p.popFloat());
            var i;
            for (i = 1; i < len; i++)
            {
                ctx.lineTo(p.popFloat(), p.popFloat());
            }
            ctx.restore();
            ctx.stroke();
        }
    }
    else if (tag == 4)
    {
        // Circle
        var r = p.popFloat();
        ctx.save();
        ctx.transform(a, b, c, d, e, f);
        ctx.beginPath();
        if (r > 0) ctx.arc(0, 0, r, 0, 2 * Math.PI);
        ctx.restore();
        ctx.stroke();
    }
    else if (tag == 5)
    {
        // ThickCircle
        var r = p.popFloat();
        var w = Math.abs(p.popFloat());
        ctx.save();
        ctx.transform(a, b, c, d, e, f);
        if (w > 0) ctx.lineWidth = w;
        ctx.beginPath();
        if (r > 0) ctx.arc(0, 0, r, 0, 2 * Math.PI);
        if (w > 0)
        {
            ctx.stroke();
            ctx.restore();
        }
        else
        {
            ctx.restore();
            ctx.stroke();
        }
    }
    else if (tag == 6)
    {
        // Text
        var len = p.popWord32();
        var txt = p.popUtf8(len);

        ctx.save();
        ctx.transform(a, b, c, d, e, f);
        ctx.scale(1,-1);
        ctx.fillText(txt, 0, 0);
        ctx.restore();
    }
    else if (tag == 7)
    {
        // Bitmap
        var len = p.popWord32();
        var dat = "data:image/png;base64," + atob(p.popBytes(len));
        ctx.save();
        ctx.transform(a, b, c, d, e, f);
        var img = new Image(dat);
        ctx.drawImage(img,
            -img.naturalWidth / 2, -img.naturalHeight / 2,
            img.naturalWidth, img.naturalHeight);
        ctx.restore();
    }
    else if (tag == 8)
    {
        // Color
        var cr = p.popByte();
        var cg = p.popByte();
        var cb = p.popByte();
        var ca = p.popByte();
        var str = "rgba(" + cr + "," + cg + "," + cb + "," + ca + ")";
        ctx.save();
        ctx.strokeStyle = str;
        ctx.fillStyle = str;
        display(ctx, p, a, b, c, d, e, f);
        ctx.restore();
    }
    else if (tag == 9)
    {
        // Translate
        var x = p.popFloat();
        var y = p.popFloat();
        display(ctx, p,
            a, b, c, d,
            a * x + c * y + e,
            b * x + d * y + f);
    }
    else if (tag == 10)
    {
        // Rotate
        var r = p.popFloat();
        var th = Math.PI * r / 180;
        display(ctx, p,
             a * Math.cos(th) + c * Math.sin(th),
             b * Math.cos(th) + d * Math.sin(th),
            -a * Math.sin(th) + c * Math.cos(th),
            -b * Math.sin(th) + d * Math.cos(th),
            e, f);
    }
    else if (tag == 11)
    {
        // Scale
        var x = p.popFloat();
        var y = p.popFloat();
        display(ctx, p,
            x * a, x * b,
            y * c, y * d,
            e, f);
    }
    else if (tag == 12)
    {
        while (p.peekByte() != 0)
        {
            display(ctx, p, a, b, c, d, e, f);
        }

        p.popByte(); // Discard trailing zero
    }
}

/*
 * Initialization.  Examines the variables defined at the top level and
 * sets up the drawing, streaming, and event handling as appropriate.
 */
function init()
{
    var canvas = document.getElementById("screen");

    if (window.picture)
    {
        displayInCanvas(canvas, picture);
    }

    if (window.streamURI)
    {
        var eventSource = new EventSource(streamURI);
        eventSource.onmessage = function(event) {
            displayInCanvas(canvas, event.data);
        }
    }

    if (window.eventURI)
    {
        var fireEvent = function(str)
        {
            var xhr = new XMLHttpRequest();
            xhr.open("POST", window.eventURI + str, true);
            xhr.send(null);
        };

        var lastMove = 0;
        var lastKeyCode = -1;

        var states = function(e)
        {
            var s = e.shiftKey ? "1" : "0";
            var a = e.altKey   ? "1" : "0";
            var c = e.ctrlKey  ? "1" : "0";
            return "&shift=" + s + "&alt=" + a + "&ctrl=" + c;
        };

        /*
         * Keys that should be recognized from the JavaScript key property, if
         * and when major browsers ever implement it.
         */
        var specialKeys = [
            "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10", "F11", "F12",
            "Up", "Down", "Left", "Right", "PageUp", "PageDown", "Home", "End", "Insert",
            "Del", "NumLock"
            ];

        /*
         * Keys that should be recognized from the JavaScript keyCode property.
         * These are actually, by some miracle, consistent across browsers.
         */
        var specialKeyCodes = [
            112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123,
            38, 40, 37, 39, 33, 34, 36, 35, 45, 46, 144
            ]

        /*
         * Since Firefox doesn't deliver key codes with the keypress event, we
         * remember the last code from a keydown event, in the hopes that it
         * can be matched up, and we can deliver the event for releasing a
         * char-type key.
         */
        var lastKeyCode;

        /*
         * Cached map from key codes to characters.  This is so that when the
         * key is released, we can deliver the right event with the right
         * character even though several browsers don't fill in the charCode
         * property.
         */
        var cachedCodes = {};

        /*
         * Look up a special key from a key event, and return the identifier
         * string, or null if there is none.
         */
        var getSpecialKey = function(e)
        {
            if (e.key)
            {
                var i = specialKeys.indexOf(e.key);

                if (i == -1) return null;
                else         return e.key;
            }
            
            if (e.keyCode)
            {
                var i = specialKeyCodes.indexOf(e.keyCode);

                if (i == -1) return null;
                else         return specialKeys[i];
            }

	        return null;
        };

        window.onkeydown = function(e) {
            lastKeyCode = e.keyCode;

        	var kname = getSpecialKey(e);
        	if (kname == null) return true;

            fireEvent("&type=k&btn=" + kname + "&state=1&x=0&y=0" + states(e));

			if (e.preventDefault) e.preventDefault();
			if (e.stopPropogation) e.stopPropogation();
            if (e.cancelBubble) e.cancelBubble();
			return false;
        };

        window.onkeypress = function(e)
        {
            if (e.charCode == 0) return true;

            cachedCodes[lastKeyCode.toString()] = String.fromCharCode(e.charCode);

        	var kname = encodeURIComponent(String.fromCharCode(e.charCode));
            fireEvent("&type=k&btn=" + kname + "&state=1&x=0&y=0" + states(e));

            if (e.preventDefault) e.preventDefault();
            if (e.stopPropogation) e.stopPropogation();
            if (e.cancelBubble) e.cancelBubble();
            return false;
        };

        window.onkeyup = function(e) {
            var kname = getSpecialKey(e);

            if (kname == null && e.charCode && e.charCode != 0)
            {
                kname = encodeURIComponent(String.fromCharCode(e.charCode));
            }

            if (kname == null && e.keyCode
                && cachedCodes.hasOwnProperty(e.keyCode.toString()))
            {
                kname = encodeURIComponent(cachedCodes[e.keyCode.toString()]);
            }

            if (kname == null) return true;

            fireEvent("&type=k&btn=" + kname + "&state=0&x=0&y=0" + states(e));

            if (e.preventDefault) e.preventDefault();
            if (e.stopPropogation) e.stopPropogation();
            if (e.cancelBubble) e.cancelBubble();
            return false;
        };

        canvas.onmousedown = function(e) {
            canvas.focus();

            var box = canvas.getBoundingClientRect();
            var x = e.clientX - box.left - 250;
            var y = 250 - e.clientY + box.top;

            var btn;
            if      (e.button == 0) btn = "lbtn";
            else if (e.button == 1) btn = "mbtn";
            else if (e.button == 2) btn = "rbtn";
            else return true;

            fireEvent("&type=k&btn=" + btn + "&state=1&x=" + x + "&y=" + y + states(e));

            if (e.preventDefault) e.preventDefault();
            if (e.stopPropogation) e.stopPropogation();
            if (e.cancelBubble) e.cancelBubble();
            return false;
        };

        canvas.onmouseup = function(e) {
            var box = canvas.getBoundingClientRect();
            var x = e.clientX - box.left - 250;
            var y = 250 - e.clientY + box.top;

			var btn;
			if      (e.button == 0) btn = "lbtn";
			else if (e.button == 1) btn = "mbtn";
			else if (e.button == 2) btn = "rbtn";
			else return true;

            fireEvent("&type=k&btn=" + btn + "&state=0&x=" + x + "&y=" + y + states(e));

            if (e.preventDefault) e.preventDefault();
            if (e.stopPropogation) e.stopPropogation();
            if (e.cancelBubble) e.cancelBubble();
            return false;
        };

        canvas.onmousemove = function(e) {
            var t = Date.now();
            if (lastMove > t - 100) return true;
            lastMove = t;

            var box = canvas.getBoundingClientRect();
            var x = e.clientX - box.left - 250;
            var y = 250 - e.clientY + box.top;

            fireEvent("&type=m&x=" + x + "&y=" + y);

            if (e.preventDefault) e.preventDefault();
            if (e.stopPropogation) e.stopPropogation();
            if (e.cancelBubble) e.cancelBubble();
            return false;
        };

        canvas.focus();
        window.onfocus = function() { canvas.focus(); }
    }
    else
    {
        canvas.onmouseover = function() { tooltip.show(); };
        canvas.onmouseout  = function() { tooltip.hide(); };
        canvas.onclick     = function() { tooltip.pin();  };
    }
}

