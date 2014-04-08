import cdb

inputfile = "morepork-dropout-3e116ed1-i15-h399-o2-b1-8000Hz-w512.net"
db = cdb.init(inputfile)

for k in db.keys():
    print k
