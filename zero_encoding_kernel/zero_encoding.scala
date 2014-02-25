import scalapipe.dsl._
import scalapipe.kernels._

object ZeroEncode extends App {


	//write Kernel
	val WriteFile = new Kernel
	{
		val in = input(UNSIGNED32)
		val fname = config(STRING,'filename)
		val temp = local(UNSIGNED32)

		val fd = local(stdio.FILEPTR,0)

		if(fd == 0) 
		{
			fd = stdio.fopen(fname,"wb")
			if (fd == 0) 
			{
				stdio.printf("ERROR: could not open %s\n", fname)
				stdio.exit(-1)
			}
		}

		temp = in
		stdio.fprintf(fd, "%x \n", temp)

	}
	
	//count kernel
	val CountConsecutive = new Kernel
	{
		//initialization
		val pixel_processed = local(UNSIGNED32,0)
		val frame_size = local(UNSIGNED32,100)
		val start = local(UNSIGNED32,0)
		val pre_inte = local(UNSIGNED32,0)
		val cur_inte = local(UNSIGNED32,0)
		val count = local (UNSIGNED32,0)
		//var wrt = new write(UNSIGNED32,STRING)
		val pixelIn = input(UNSIGNED32)
		val result = output(UNSIGNED32)
		
		if(start == 0) //reading in first pixel
		{
			pre_inte = pixelIn
			pixel_processed = 1
			count = 1
			start = 1
		}
		//read in next pixel and compare to previous pixel
		cur_inte = pixelIn
		pixel_processed = pixel_processed + 1
    
    if(cur_inte == pre_inte)
    {
      count = count + 1
    }
    else // if (cur != prev)
    {
      // send out the count so far and the prev char
      result = (pre_inte << 16) + count
      
      // start a new counter
      pre_inte = cur_inte
      count = 1
    }
    // If the last pixel - send the output
    if(pixel_processed == frame_size)
    {
      result = (pre_inte << 16) + count
      start = 0
    }
  }
  
	val dropSize = new Drop(SIGNED32)

	val app = new Application
	{
			
		//val in = BMPReader('file -> "../white.bmp")
    //val in = BMPReader('file -> "../black.bmp")
    val in = BMPReader('file -> "../blackandwhite.bmp")
    
		//wrt(out,"compressed")
		dropSize(in(1))
		dropSize(in(2))
		val result = CountConsecutive(in(0))
		WriteFile(result,'filename -> "compressed.txt")

	}
	app.emit("zero_encoding")
}

