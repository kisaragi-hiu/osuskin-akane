akane.osk: akane.zip
	mv akane.zip akane.osk

akane.zip: .out
	cd .out && 7z a ../akane.zip *
