#pragma once
// transport
#include "zmq.hpp"
// protobuf generated
#include "search_request.pb.h"

// define an io loop that dispatches messages and replies for simple rpc
// based on zmq req/rep
namespace molemind { namespace sdm { namespace io {

  using namespace molemind;

  
  // TODO: avoid message copying
  
  inline void send_reply(const std::string& rep, zmq::socket_t& sock, const bool tracing=false) {
    //
    if (tracing) std::cerr << rep << std::endl;
    //
    std::size_t replen = rep.size();
    zmq::message_t reply(replen);
    memcpy(static_cast<void*>(reply.data()), rep.c_str(), replen);
    sock.send(reply);
  }
  
  // TODO template on search_request
  
  int inline receive_request(const zmq::message_t& request, search_request& search) {
    std::string msg_str(static_cast<const char*>(request.data()), request.size());
    return search.ParseFromString(msg_str);
  }
  
  
  
  inline int request_reply(const std::string& endpoint) {
    
    // set number of i/o threads
    zmq::context_t context(4);
    
    // setup endpoints
    zmq::socket_t receiver(context, ZMQ_REP);
    receiver.bind(endpoint.c_str());
    
    zmq::message_t request;
    std::string reply;
    
    
    // single thread io loop uses select like semantics to check which socket has data
    
    while (true) {
      
      try {
        
        receiver.recv(&request);
        
        // do summat wi message like dispatch it!
        reply = "good";
        // } catch (application exception) {
        
        
      } catch (zmq::error_t const& z) {
        std::cerr << "zmq error:" << z.what() << std::endl;
        //rep = utl::format_string(z.what(), "exception");
        send_reply(z.what(), receiver, false);
        break;
        
      } catch (std::exception const& t) {
        std::cerr << "exception:" << t.what() << std::endl;
        //rep = utl::format_string(t.what(), "exception");
        send_reply(t.what(), receiver, false);
        break;
      }
      
      // XXX always send a reply message
      send_reply(reply, receiver, true);
      
    } // mainloop
    
    // break to here...
    receiver.close();
    context.close();
    return(1);
    
  }

  struct message {
    // one function we provide which is to dispatch messages to a single database instance
    int virtual message_loop(const std::string& endpoint, const sdm::database& db) = 0;
    
  };

  struct search_message : message {
    int inline message_loop(const std::string& endpoint, const sdm::database& db) {
      return request_reply(endpoint);
    }
  };
  
  
 
 }}}

  
