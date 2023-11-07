echo "  $ alias run='$1 -impl - | ocamlformat - --impl'";
echo "";
cat ./example.ml | grep '^type' | while read line; do
  echo '  $ cat <<"EOF" | run';
  echo "  > $line";
  echo '  > EOF';
  echo '';
done
