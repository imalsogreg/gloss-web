/*
 * Tooltip function by Michael Leigeber, modified by Chris Smith
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
