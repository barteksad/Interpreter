TMP_OUT=$(mktemp)
TMP_ERR=$(mktemp)

allok=true

for file in ./good/*.fat
do
	echo "$file"

	./interpreter $file > $TMP_OUT 2> $TMP_ERR 

	if diff ${file%fat}out $TMP_OUT >/dev/null 
	then 
		echo  "OK"
	else 
		echo  "ERROR IN OUTPUT" 
		allok=false
	fi

	if diff ${file%fat}err $TMP_ERR >/dev/null 
	then 
		echo  "OK"
	else 
		echo  "ERROR IN ERROR"
		allok=false
	fi

	echo ""
done

for file in ./bad/*.fat
do
	echo  "$file"
	
	./interpreter $file > $TMP_OUT 2> $TMP_ERR
	
	if diff ${file%fat}out $TMP_OUT>/dev/null
	then 
		echo  "OK"
	else 
		echo  "ERROR IN OUTPUT"
		allok=false
	fi

	if diff ${file%fat}err $TMP_ERR>/dev/null
	then 
		echo  "OK"
	else 
		echo  "ERROR IN ERROR"
		allok=false
	fi

	echo ""
done

if $allok
then
	echo  "ALL TESTS PASSED"
else
	echo  "SOME TESTS FAILED"
fi

rm $TMP_OUT
rm $TMP_ERR