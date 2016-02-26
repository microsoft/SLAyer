/*
  Copyright (c) Microsoft Corporation.  All rights reserved.


              +-------+
  writer ---> |mailbox| ---> reader
              +-------+

  writer tells reader of mailbox via channel.

  mailbox should be in writer's local-heap.
  It should also be in reader's local-heap: if we take account of expr equality when
  calculating reachability.

  This program should be SAFE: reader should not have mailbox framed-away.
*/

int mailbox;
int lock = 0;

void writer(int** channel)
{
  lock = 1;
  mailbox = 1;
  *channel = &mailbox;
  lock = 0;
}

// mailbox \in footprint.
void reader(int* channel)
{
  int x;
  lock = 1;
  x = *channel;
  lock = 0;
}

void main()
{
  int* channel;

  writer(&channel);
  // assert ( (channel == &mailbox) && (mailbox == 1) )
  reader(channel);
}
