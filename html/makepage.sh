funs=`grep -oP '//\* fig(..)' chapter3.html | cut -c5,6,7,8,9,10,11,12`
for f in $funs
    do
	url='chapter3.html?fig='$f
	echo $url
	chromium-browser --headless --disable-gpu --print-to-pdf=figs/$f.pdf $url
    done

