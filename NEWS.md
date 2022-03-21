# medExtractR 0.4

- Allow generic dictionaries for tapering functions
- Extra string matching functions to reduce drug list; see "Customizing drug_list" vignette

# medExtractR 0.2

- Change how "dose" (i.e., dose given intake) is extracted in some cases. Previously, expressions like "lamotrigine 300-200" which indicate 300mg taken in the morning and 200mg taken in the evening would be extracted separately as *300* and *200*. Now, these are extracted as the full expression *300-200* to preserve contextual information and allow for easier dose calculation when processing raw output.
- Add `package="medExtractR"` to `data` calls within functions 
- Update some data files to allow for more flexibility in phrases (e.g., optional spaces in frequency expressions like *qday*)
- Minor bug fixes

# medExtractR 0.1

Initial release
