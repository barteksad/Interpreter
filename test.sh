TMP_OUT=$(mktemp)
TMP_ERR=$(mktemp)

for file in ./good/*.myl
do
        ./interpreter $file > $TMP_OUT 2> $TMP_ERR

        if diff ${file%myl}out $TMP_OUT>/dev/null
        then echo -e "OK";
        else echo -e "ERROR IN OUTPUT"

        if diff ${file%myl}err $TMP_ERR>/dev/null
        then echo -e "OK";
        else echo -e "ERROR IN ERROR"

        echo ""
done