#! /bin/bash
ctags-exuberant -R --extra=+fq -e --langdef=scala --langmap=scala:.scala --langdef=otq --langmap=otq:.otq --langdef=xml --langmap=xml:.xml --langdef=properties --langmap=properties:.properties "$@"
