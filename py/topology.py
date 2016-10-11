#
# python client for topology microservice
# copyright (c) 2016 Simon Beaumont. All Rights Reserved.

import zmq

def run_it():
    
    #  Prepare our context and sockets
    context = zmq.Context()
    socket = context.socket(zmq.REQ)
    socket.connect("tcp://localhost:5555")

    #  Do 10 requests, waiting each time for a response
    for request in range(1,21):
        mess = "words\twords\tsimon\t0.0\t0.5\t27\n".format(request)
        #print(">", mess)
        socket.send(mess.encode('utf-8'))
        reply = socket.recv()
        print("<", reply)
