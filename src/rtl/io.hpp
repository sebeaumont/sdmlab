#pragma once
// transport
#include "zmq.hpp"

// define an io loop that dispatches messages and replies for simple rpc
// based on zmq req/rep
namespace molemind { namespace sdm { namespace io {

  using namespace molemind;

  
  // low level zmq string io
  // TODO: avoid message copying
  
  inline void send_reply(const std::string& rep, zmq::socket_t& sock, const bool tracing=false) {
    if (tracing) std::cerr << rep << std::endl;
    std::size_t replen = rep.size();
    zmq::message_t reply(replen);
    memcpy(static_cast<void*>(reply.data()), rep.c_str(), replen);
    sock.send(reply);
  }
  

  // generic message handlers
  
  template <typename T> int inline parse_request(const zmq::message_t& request, T& buffer) {
    std::string msg_str(static_cast<const char*>(request.data()), request.size());
    std::cerr << "RX:" << msg_str << std::endl;
    return buffer.parseFromString(msg_str);
  }
  
  
  template <typename T> int inline message_rpc_server(const std::string& endpoint, sdm::database& db, unsigned n_threads=4) {
    
    // set number of i/o threads
    zmq::context_t context(n_threads);
    
    // setup endpoints
    zmq::socket_t receiver(context, ZMQ_REP);
    receiver.bind(endpoint.c_str());
    
    zmq::message_t request;
    std::string reply;
    
    
    // single thread/endpoint io loop
    
    while (true) {
      
      try {
        
        receiver.recv(&request);
        T buffer;
        
        if (parse_request<T>(request, buffer)) {
        
          reply = buffer.dispatch(db);
          // echo server!
          //reply = buffer.toString();
          
        // do summat wi message like dispatch it!
        } else {
          reply = "bad message";
        }
        
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

  
  
 
 }}}

  
