namespace Oculus.Platform
{
  public sealed class Request<T> : Request
  {
    public Request(ulong requestID) : base (requestID) { }

    public Request<T> OnComplete(Message<T>.Callback callback)
    {
      Callback.OnComplete<T>(this, callback);
      return this;
    }
  }

  public class Request
  {
    public Request(ulong requestID) {this.RequestID = requestID;}
    public ulong RequestID {get; set;}

    public Request OnComplete(Message.Callback callback)
    {
      Callback.OnComplete(this, callback);
      return this;
    }

    /**
     * This will run callbacks on all messages that returned from the server.
     * If too many message are coming back at once, then a limit can be passed in
     * as an arg to limit the number of messages to run callbacks on at a time
     */
    public static void RunCallbacks(uint limit = 0)
    {
      // default of 0 will run callbacks on all messages on the queue
      if (limit == 0)
      {
        Callback.RunCallbacks();
      }
      else
      {
        Callback.RunLimitedCallbacks(limit);
      }
    }
  }
}
