#ifndef GETDATA_LEGACY_H
#define GETDATA_LEGACY_H

/* Sanity check */
#ifndef GETDATA_H
#error "Never use <getdata_legacy.h> directly; include <getdata.h> instead."
#endif

/***************************************************************************/
/*                                                                         */
/*    getdata_error_string: Write a descriptive message in the supplied    */
/*    buffer describing the last library error associated with the         */
/*    supplied buffer.  The message may be truncated but will be null      */
/*    terminated.  The deprecated, non-reentrant version                   */
/*    (GetDataErrorString) will report the error from the last old-style   */
/*    error encountered, but not any from calls to the new DIRFILE* API.   */
/*                                                                         */
/*      dirfile: the dirfile to check for a library error                  */
/*      buffer : memory into which to write the string                     */
/*      buflen : length of the buffer.  GetDataErrorString will not write  */
/*               more than buflen characters (including the trailing '\0') */
/*                                                                         */
/*    return value: buffer or NULL if buflen < 1                           */
/*                                                                         */
/***************************************************************************/
char* GetDataErrorString(char* buffer, size_t buflen)
  __attribute__ ((__deprecated__));

/***************************************************************************/
/*                                                                         */
/*    getdata: Retrieve the specified vector from the supplied dirfile.    */
/*    The deprecated GetData does the dirfile look-up by directory name    */
/*                                                                         */
/*      dirfile/dirfilename: The dirfile to query                          */
/*      field_code         : The field to return                           */
/*      first_frame, first_samp: the first sample read is                  */
/*                first_samp + samples_per_frame * first_frame             */
/*      num_frames, num_samps: the number of samples read is               */
/*                num_samps + samples_per_frame * num_frames               */
/*      return_type: data type of *data_out:                               */
/*           GD_INT8,  GD_INT16,   GD_INT32,  GD_INT64                     */
/*          GD_UINT8, GD_UINT16,  GD_UINT32, GD_UINT64                     */
/*                               GD_FLOAT32, GD_FLOAT64                    */
/*      data_out: pointer to a sufficiently large area of memory to        */
/*           write the data                                                */
/*      error_code: error status of the old-style API                      */
/*                                                                         */
/*    return value: number of samples actually retrieved                   */
/*                                                                         */
/***************************************************************************/
int GetData(const char *dirfilename, const char *field_code, int first_frame,
    int first_samp, int num_frames, int num_samp, gd_type_t return_type,
    void *data_out, int *error_code) __attribute__ ((__deprecated__));

/***************************************************************************/
/*                                                                         */
/*    get_n_frmaes: Return the number of frames in the supplied dirfile.   */
/*                                                                         */
/*      dirfile/dirfilename: The dirfile to query                          */
/*      error_code         : error status of the old-style API             */
/*      unused             : unused                                        */
/*                                                                         */
/*    return value: the number of frames in the dirfile, or zero on error  */
/*                                                                         */
/***************************************************************************/
int GetNFrames(const char *dirfilename, int *error_code, const void *unsued)
  __attribute__ ((__deprecated__));


/***************************************************************************/
/*                                                                         */
/*    get_samples_per_frame: Return the number of samples per frame for    */
/*    the given field in the supplied dirfile.                             */
/*                                                                         */
/*      dirfile/dirfilename: The dirfile to query                          */
/*      field_code         : The field in question                         */
/*      error_code         : error status of the old-style API             */
/*                                                                         */
/*    return value: the number of samples per frame for the given field    */
/*      or zero on error                                                   */
/*                                                                         */
/***************************************************************************/
int GetSamplesPerFrame(const char *dirfilename, const char *field_code,
    int *error_code) __attribute__ ((__deprecated__));

/***************************************************************************/
/*                                                                         */
/*    putdata: Store the specified vector from the supplied dirfile.       */
/*    The deprecated GetData does the dirfile look-up by directory name    */
/*                                                                         */
/*      dirfile/dirfilename: The dirfile to query                          */
/*      field_code         : The field to store                            */
/*      first_frame, first_samp: the first sample written is               */
/*                first_samp + samples_per_frame * first_frame             */
/*      num_frames, num_samps: the number of samples written is            */
/*                num_samps + samples_per_frame * num_frames               */
/*      return_type: data type of *data_in:                                */
/*           GD_INT8,  GD_INT16,   GD_INT32,  GD_INT64                     */
/*          GD_UINT8, GD_UINT16,  GD_UINT32, GD_UINT64                     */
/*                               GD_FLOAT32, GD_FLOAT64                    */
/*      data_in: pointer to the data to write                              */
/*      error_code: error status of the old-style API                      */
/*                                                                         */
/*    return value: number of samples actually stored                      */
/*                                                                         */
/***************************************************************************/
int PutData(const char *filename_in, const char *field_code, int first_frame,
    int first_samp, int num_frames, int num_samp, gd_type_t data_type,
    void *data_in, int *error_code) __attribute__ ((__deprecated__));

#endif
