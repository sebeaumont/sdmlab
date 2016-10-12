#include <sstream>

namespace molemind { namespace sdm { namespace io {
  struct topology_request {
    std::string tgt_space;
    std::string src_space;
    std::string src_name;
    double p_lower;
    double r_upper;
    std::size_t max_card;
    
    inline const bool parseFromString(std::string& s) {
      std::stringstream buff(s);
      buff >> tgt_space >> src_space >> src_name >> p_lower >> r_upper >> max_card;
      return buff.good() ? true : false;
    }
    
    inline std::string toString() {
      std::stringstream buff;
      buff << tgt_space << "\t" << src_space << "\t" << src_name << "\t" << p_lower << "\t" << r_upper << "\t" << max_card;
      return buff.str();
    }
    
    inline std::string dispatch(molemind::sdm::database& db) {
      boost::optional<database::space::topology> t = db.neighbourhood(tgt_space, src_space, src_name, p_lower, r_upper, max_card);

      if (t) {
        std::stringstream reply;
        for (auto p : *t) reply << p << '\n';
        return reply.str();
      } else {
        return "no";
      }
    }
    
  };
  
  // to own header please...
  struct topology_reply {
    inline std::string toString() {
      return "OK";
    }
  };
  
}}}
