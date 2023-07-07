using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using Deltares.UGrid.Helpers;

namespace Deltares.UGrid.Api
{
    /// <summary>
    /// Base class for disposable mesh objects. Provides pinning of arrays in memory for exchange with native calls.
    /// </summary>
    public abstract class DisposableMeshObject : IDisposable
    {
        private readonly Dictionary<object,GCHandle> objectGarbageCollectHandles = new Dictionary<object, GCHandle>();
        private bool disposed;

        /// <summary>
        /// Disposes the unmanaged resources
        /// </summary>
        ~DisposableMeshObject()
        {
            Dispose(false);
        }

        /// <summary>
        /// Indicates if arrays are pinned in memory
        /// </summary>
        protected bool IsMemoryPinned
        {
            get { return objectGarbageCollectHandles.Count > 0; }
        }

        /// <inheritdoc/>
        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }
        
        /// <summary>
        /// Get the pointer to the pinned object
        /// </summary>
        /// <param name="objectToLookUp">Object to get </param>
        /// <returns></returns>
        protected IntPtr GetPinnedObjectPointer(object objectToLookUp)
        {
            if (!IsMemoryPinned)
            {
                PinMemory();
            }

            return objectGarbageCollectHandles[objectToLookUp].AddrOfPinnedObject();
        }

        /// <summary>
        /// Pins the arrays in memory (no garbage collect until unpinned (done in dispose))
        /// </summary>
        protected void PinMemory()
        {
            var arrayFields = GetType().GetFields().Where(f => f.FieldType.IsArray);
            
            // force initialization
            foreach (var arrayField in arrayFields)
            {
                var elementType = arrayField.FieldType.GetElementType();
                var objectToPin = arrayField.GetValue(this);

                if (objectToPin == null)
                {
                    objectToPin = Array.CreateInstance(elementType, 0);
                    arrayField.SetValue(this, objectToPin);
                }

                if (elementType == typeof(string))
                {
                    var bufferSize = GetType().GetBufferSize(arrayField.Name);
                    if (bufferSize == 0) continue;

                    var bytes = ((string[])objectToPin).GetFlattenedAsciiCodedStringArray(bufferSize);
                    AddObjectToPin(bytes, objectToPin);
                }
                else
                {
                    AddObjectToPin(objectToPin);
                }
            }
        }

        /// <summary>
        /// Disposes resources
        /// </summary>
        /// <param name="disposing">Boolean indicating that the call is called from the dispose method
        /// not the destructor</param>
        protected virtual void Dispose(bool disposing)
        {
            if (disposed)
                return;

            ReleaseUnmanagedResources();

            disposed = true;
        }

        private void UnPinMemory()
        {
            foreach (var valuePair in objectGarbageCollectHandles)
            {
                valuePair.Value.Free();
            }

            objectGarbageCollectHandles.Clear();
        }

        private void AddObjectToPin(object objectToPin, object lookupObject = null)
        {
            var key = lookupObject ?? objectToPin;
            objectGarbageCollectHandles.Add(key, GCHandle.Alloc(objectToPin, GCHandleType.Pinned));
        }

        private void ReleaseUnmanagedResources()
        {
            UnPinMemory();
        }
    }
}