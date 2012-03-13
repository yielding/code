#ifndef HFS_KEY_H_BLI35TKP
#define HFS_KEY_H_BLI35TKP

#include <stdint.h>
#include <set>
#include <string>

#include <openssl/aes.h>
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
struct HFSKey
{
    enum { kDecrypt = 1, kEncrypt = 2};

    HFSKey(uint32_t sz=32);
    HFSKey(std::string const& s, uint32_t sz=32, int dir=kDecrypt);
    HFSKey(HFSKey const& rhs);

    bool operator<(HFSKey const& rhs) const;

    void set_decrypt(uint8_t* fk_, uint32_t sz=32);
    void set_encrypt(uint8_t* fk_, uint32_t sz=32);

    auto size()      -> size_t   { return m_size;   }
    auto as_aeskey() -> AES_KEY& { return m_aeskey; }
    auto as_buffer() -> uint8_t* { return m_fk;     }
    auto to_str() const -> std::string;

private:
    AES_KEY  m_aeskey;
    uint32_t m_size;
    uint8_t  m_fk[32];
};

typedef std::set<HFSKey> HFSKeys;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
