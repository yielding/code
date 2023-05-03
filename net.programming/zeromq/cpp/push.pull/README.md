## push / pull pattern

#### 1. Intro
> As a networking glue of big data processing, S/W, this pattern should be considered as a proper tool.

- Produer(push) / Consumer(pull) pattern
- Producer distributes messages to multiple workers in a `pipeline`
- Data always flows down the pipeline, and each stage of the pipelne is connected to at least one node.
- When a pipeline stage is connected to multiple nodes data is `load-balanced` among all connected nodes.
- [URL](https://learning-0mq-with-pyzmq.readthedocs.io/en/latest/pyzmq/patterns/pushpull.html)
   of push / pull or image here <img src="README.assets/pushpull.webp" alt="push-pull" style="zoom: 100%;" />

#### 2. Run
 ```bash
 $./push # start server(push)
 
 $./pull # start clients(pull)
 ```
 
#### 3. Contact
- mail me to [yielding@gmail.com] (mailto:yielding@gmail.com).
