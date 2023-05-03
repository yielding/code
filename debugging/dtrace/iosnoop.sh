#!/usr/sbin/dtrace -qs
BEGIN
{
    printf("%16s %5s %40s %10s %2s %7s\n", "COMMAND", "PID", "FILE",
    "DEVICE", "RW", "MS");
}

io:::start
{
    start[args[0]->b_edev, args[0]->b_blkno] = timestamp;
    command[args[0]->b_edev, args[0]->b_blkno] = execname;
    mypid[args[0]->b_edev, args[0]->b_blkno] = pid;
}

io:::done
/start[args[0]->b_edev, args[0]->b_blkno]/
{
    elapsed = timestamp - start[args[0]->b_edev, args[0]->b_blkno];
    printf("%16s %5d %40s %10s %2s %3d.%03d\n", command[args[0]->b_edev, args[0]->b_blkno],
    mypid[args[0]->b_edev, args[0]->b_blkno], args[2]->fi_pathname, args[1]->dev_statname,
    args[0]->b_flags&B_READ? "R": "W", elapsed/1000000, (elapsed/1000)%1000);
    start[args[0]->b_edev, args[0]->b_blkno] = 0;
    command[args[0]->b_edev, args[0]->b_blkno] = 0;
    mypid[args[0]->b_edev, args[0]->b_blkno] = 0;
}
