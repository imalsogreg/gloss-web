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

var ctx = null;

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
        ctx.arc(0, 0, r, 0, 2 * Math.PI);
        ctx.restore();
        ctx.stroke();
    }
    else if (tag == 5)
    {
        // ThickCircle
        var r = p.popFloat();
        var w = p.popFloat();
        ctx.save();
        ctx.transform(a, b, c, d, e, f);
        if (w > 0) ctx.lineWidth = w;
        ctx.beginPath();
        ctx.arc(0, 0, r, 0, 2 * Math.PI);
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

