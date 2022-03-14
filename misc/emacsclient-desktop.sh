#!/bin/sh

desktop_dir=/usr/local/share/applications
mkdir -p $desktop_dir
cat > $desktop_dir/emacsclient.desktop <<EOF
[Desktop Entry]
Name=Emacs Client
GenericName=Text Editor
Comment=Edit text
MimeType=text/english;text/plain;text/x-makefile;text/x-c++hdr;text/x-c++src;text/x-chdr;text/x-csrc;text/x-java;text/x-moc;text/x-pascal;text/x-tcl;text/x-tex;application/x-shellscript;text/x-c;text/x-c++;
Exec=emacsclient -ca '' %f
Icon=emacs
Type=Application
Terminal=false
Categories=Utility;TextEditor;
EOF

update-desktop-database $desktop_dir
