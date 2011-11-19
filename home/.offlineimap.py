import os

def get_password(account):
    authinfo = os.popen("gpg -q --no-tty -d ~/.muttpriv/%s.pass.gpg" % (account)).read()
    return authinfo.strip()
