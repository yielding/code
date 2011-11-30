# emf          : plist, effaceable area
# dkey         : plist,  ?
# class_keys   : plist,  ?
# m_class_keys : acquired using DKEY, class_keys

# wrapped_file_key, protecttion_class 
#              : attribute_tree
# file_key     : wrapped_file_key, protection_class


def carve_undelete_file_info
  files = catalog_tree.search_using_journal(leaf_file_record, journal)
  
  for leaf in attribute_tree.search_using_journal
     wrapped_file_key, protection_class = attribute_tree.search_using_journal(leaf_filekey_record, journal)
     file_key = unwrap_using_class_keys_acquired_from_plist(wrapped_file_key, protection_class)
     file_keys << file_key
  end
  
  return files, file_keys
end

def undelete
  files, file_keys = carve_undelete_file_info
  for file in files
    file_key = find(file_keys, for_this: file)
    buffer = decrypt_file_blocks(catalog_data, file_key)    
    if (decrypted_ok(buffer))
      ofs = OFtream.new
      ofs.write(buffer)
    end
  end
end