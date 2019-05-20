(function(d,w){
	var i=j=k=0,
	id="804933fb94",
	gi="getElementById",
	target=d[gi]("bn_"+id);
		function a(){
		var s=d.createElement("script");
		s.src="//recreativ.ru/cs/1/1";
		s.onload=function(e){
			i++;
			s.parentNode.removeChild(s);
			if(k!=w.rc_cache && i<=2){
				k=w.rc_cache;
				a();
			}else{
				j=0;
				b();
			}
		};
		s.onerror=function(e){
			j++;
			s.parentNode.removeChild(s);
			if(j<=10) a();
		};
		d.getElementsByTagName("head")[0].appendChild(s);
	}
	function b(){
		var s=d.createElement("script");
		s.src="//recreativ.ru/tizers.php?bn="+id+"&cache="+(k==w.rc_cache?k:0);
		s.onload=function(e){
			i++;
			s.parentNode.removeChild(s);
		};
		s.onerror=function(e){
			j++;
			s.parentNode.removeChild(s);
			if(j<=10) setTimeout(b,100);
		};
		d.getElementsByTagName("head")[0].appendChild(s);
	}
	a();
})(document,window);
