(load "Cocoa")
(load "Quartz")

(set *micusp-folder* "../data/micusp/")
(set fm (NSFileManager defaultManager))
((fm subpathsAtPath:*micusp-folder*) each:
 (do (fn)
  (if (== (fn pathExtension) "pdf") 
   (puts fn)
   (set path (*micusp-folder* stringByAppendingString: fn))
   (set data (NSData dataWithContentsOfFile:path))
   (set pdf ((PDFDocument alloc) initWithData:data))
   (set str (pdf string))
   (str writeToFile: ((path stringByDeletingPathExtension) stringByAppendingPathExtension:"txt") atomically:YES encoding:NSUTF8StringEncoding error:nil))))