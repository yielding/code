namespace MD.memory
{
    public class DisposableBase: IDisposable
    {
        ~DisposableBase()
        {
            Dispose(false);
        }

        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }

        protected virtual void Dispose(bool disposing)
        {

        }
    }

    public class DisposableSublass: DisposableBase
    {
        protected override void Dispose(bool disposing)
        {
            try
            {
                if (disposing)
                {
                    // Clean up managed resources
                }

                // Clean up native resources
            }
            finally
            {
                base.Dispose(disposing);
            }
        }
    }
}
