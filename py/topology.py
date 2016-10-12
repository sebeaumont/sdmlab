# HACK ALERT
# python client for prototype topology microservice
# copyright (c) 2016 Simon Beaumont. All Rights Reserved.

import zmq

class Topology:

    def __init__(self, endpoint="tcp://localhost:5555"):

        self._context = zmq.Context()
        self._socket = self._context.socket(zmq.REQ)
        self._socket.connect(endpoint)


    def neighbourhood(self, target_space, source_space, source_name, plower, rupper, cupper):
        mess = "{:s}\t{:s}\t{:s}\t{:f}\t{:f}\t{:d}\n".format(target_space,
                                                             source_space,
                                                             source_name,
                                                             plower, rupper, cupper)
        self._socket.send(mess.encode('utf-8'))
        # raw bytes
        rep = self._socket.recv()
        # unicode
        reply = rep.decode('utf-8')
        # 
        lines = reply.strip().split('\n')
        #print(lines)
        data = []
        try:
            for line in lines:
                row = line.split('\t')
                data.append((str(row[0]), float(row[1]), float(row[2])))
            return(data)
        except Exception as e:
            return []
