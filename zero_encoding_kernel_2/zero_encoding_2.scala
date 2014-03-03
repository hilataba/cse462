import scalapipe.dsl._
import scalapipe.kernels._

object ZeroEncode extends App {


	//write Kernel
	val WriteFile = new Kernel ("WriteFile")
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
	val BufferFrames = new Kernel ("BufferFrames")
  {
    val pixelIn = input(UNSIGNED32)
    val output1 = output(UNSIGNED32)
    val output2 = output(UNSIGNED32)
    val deliverOutput1 = local(UNSIGNED32,1)
    val frameSize = local(UNSIGNED32,100)
    val totalNumOfPixels = local(UNSIGNED32,1600) // 16 frames in image. 100 pixels each frame
    val storeCounter = local(UNSIGNED32,0)
    val base = local(UNSIGNED32,0)
    val deliverMode = local(UNSIGNED32,0)
    val ArrayType = Vector(UNSIGNED32,1600)
    val frameArray = local(ArrayType)
    
    
    // store the pixel in a vector
    if(storeCounter <= totalNumOfPixels)
    {
      frameArray(storeCounter) = pixelIn
      storeCounter = storeCounter + 1
    }
    // Read all pixels, switch to deliver mode
    if(storeCounter == totalNumOfPixels)
    {
        deliverMode = 1
        base = 0
    }

    if(deliverMode == 1) // deliver next pixel
    {
      for(f <- 0 until 100) // frames scale
      {
        if(deliverOutput1 == 1)
        {
          for(i <- 0 until frameSize) // frame size
          {
            output1 = frameArray(base + i)
          }
          deliverOutput1 = 0
        }
        else // send to output2
        {
          for(i <- 0 until frameSize) // frame size
          {
            output2 = frameArray(base + i)
          }
          deliverOutput1 = 1
        }
        base += frameSize
        if(base == totalNumOfPixels)
          base = 0
      }
    }
  }
  
	//count kernel
	val CountConsecutive = new Kernel ("CountConsecutive")
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
  val dropPixel = new Drop(UNSIGNED32)
  
	val app = new Application
	{
			
		//val in = BMPReader('file -> "../white.bmp")
    //val in = BMPReader('file -> "../black.bmp")
    val in = BMPReader('file -> "../blackandwhite.bmp")
    
		dropSize(in(1))
		dropSize(in(2))
    val output = BufferFrames(in(0))
		val result1 = CountConsecutive(output(0))
    val result2 = CountConsecutive(output(1))
		WriteFile(result1,'filename -> "compressed1.txt")
    WriteFile(result2,'filename -> "compressed2.txt")
   // WriteFile(output(2),'filename -> "count.txt")

	}
	app.emit("zero_encoding2")
}

