#include <sstream>
#include <vector>
#include <msgpack.hpp>


namespace molemind { namespace sdm { namespace io {

  //struct topology_request;
  
  // reply message
  struct topology_reply {
    
    std::string _space;
    sdm::topology _topo;
    
    MSGPACK_DEFINE(_space, _topo);
    
    topology_reply(std::string& name, sdm::topology& top) : _space(name), _topo(top) {}
    
  };

  
  struct topology_request {
    std::string tgt_space;
    std::string src_space;
    std::string src_name;
    double p_lower;
    double r_upper;
    unsigned max_card;

    MSGPACK_DEFINE(tgt_space, src_space, src_name, p_lower, r_upper, max_card);
    
   
    inline const bool parseFromMessage(const zmq::message_t& m) {
      msgpack::sbuffer sbuf;
      sbuf.write(static_cast<const char *>(m.data()), m.size());
      msgpack::object_handle msg = msgpack::unpack(sbuf.data(), sbuf.size());
      msgpack::object obj = msg.get();
      //std::cout << obj << std::endl; // this looks reasonable but...
      
      msgpack::type::tuple<std::string, std::string, std::string, double, double, int> req;
      //std::vector<topology_request> req;
      
      try {
        obj.convert(req);
        
      } catch (...) {
        
        std::cout << "buf: ";
        for (std::size_t i = 0; i != sbuf.size(); ++i) {
          std::cout << std::hex << std::setw(2) << std::setfill('0');
          std::cout << (static_cast<int>(sbuf.data()[i]) & 0xff) << ' ';
        }
        std::cout << std::endl;
        
        std::cout << "obj: ";
        std::cout << obj << std::endl;
      }
      
      return true;
    }
    
   
    
    // message dispatch and return reply
    
    inline std::string dispatch(sdm::database& db) {
      // make call to get neighbourhood of point based on current state of object
      // -- caution to make this guarded on a sucessful parseFrom...
      
      boost::optional<sdm::topology> t = db.neighbourhood(tgt_space, src_space, src_name, p_lower, r_upper, max_card);
      if (t) {
        topology_reply rep(tgt_space, *t);
        std::stringstream sbuf;
        msgpack::pack(sbuf, rep);
        return sbuf.str();
      } else return "snafu"; // some kind of error surely?
    }
    
  };
  
  
  

  
}}}
