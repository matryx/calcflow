using UnityEngine;
using System.Linq;
using System;
using Nanome.Maths;

namespace Extensions
{
    public enum Direction
    {
        up,
        down,
        left,
        right
    }

    public static class GameObjectExtensions
    {
        /// <summary>
        /// Returns all monobehaviours (casted to T)
        /// </summary>
        /// <typeparam name="T">interface type</typeparam>
        /// <param name="gObj"></param>
        /// <returns></returns>
        public static T[] GetInterfaces<T>(this GameObject gObj)
        {
            if (!gObj) return null;
            if (!typeof(T).IsInterface) throw new SystemException("Specified type is not an interface!");
            var mObjs = gObj.GetComponents<MonoBehaviour>();

            return (from a in mObjs where a.GetType().GetInterfaces().Any(k => k == typeof(T)) select (T)(object)a).ToArray();
        }

        /// <summary>
        /// Returns the first monobehaviour that is of the interface type (casted to T)
        /// </summary>
        /// <typeparam name="T">Interface type</typeparam>
        /// <param name="gObj"></param>
        /// <returns></returns>
        public static T GetInterface<T>(this GameObject gObj)
        {
            if (!gObj) return default(T);

            if (!typeof(T).IsInterface) throw new SystemException("Specified type is not an interface!");
            return gObj.GetInterfaces<T>().FirstOrDefault();
        }

        /// <summary>
        /// Returns the first instance of the monobehaviour that is of the interface type T (casted to T)
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="gObj"></param>
        /// <returns></returns>
        public static T GetInterfaceInChildren<T>(this GameObject gObj)
        {
            if (!typeof(T).IsInterface) throw new SystemException("Specified type is not an interface!");
            return gObj.GetInterfacesInChildren<T>().FirstOrDefault();
        }

        /// <summary>
        /// Gets all monobehaviours in children that implement the interface of type T (casted to T)
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="gObj"></param>
        /// <returns></returns>
        public static T[] GetInterfacesInChildren<T>(this GameObject gObj)
        {
            if (!gObj) return null;

            if (!typeof(T).IsInterface) throw new SystemException("Specified type is not an interface!");

            var mObjs = gObj.GetComponentsInChildren<MonoBehaviour>();

            return (from a in mObjs where a.GetType().GetInterfaces().Any(k => k == typeof(T)) select (T)(object)a).ToArray();
        }

        /// <summary>
        /// Gets all monobehaviours in parents that implement the interface of type T (casted to T)
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="gObj"></param>
        /// <returns></returns>
        public static T[] GetInterfacesInParents<T>(this GameObject gObj)
        {
            if (!gObj) return null;

            if (!typeof(T).IsInterface) throw new SystemException("Specified type is not an interface!");

            var mObjs = gObj.GetComponentsInParent<MonoBehaviour>();

            return (from a in mObjs where a.GetType().GetInterfaces().Any(k => k == typeof(T)) select (T)(object)a).ToArray();
        }

        /// <summary>
        /// Returns the first instance of the monobehaviour that is of the interface type T (casted to T)
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="gObj"></param>
        /// <returns></returns>
        public static T GetInterfaceInParent<T>(this GameObject gObj)
        {
            if (!gObj) return default(T);

            if (!typeof(T).IsInterface) throw new SystemException("Specified type is not an interface!");
            return gObj.GetInterfacesInParents<T>().FirstOrDefault();
        }

        public static void MoveTo(this GameObject gObj, Vector3 destination, float seconds)
        {
            LerpMover.LerpMove(gObj, destination, seconds);
        }

    }

    public class LerpMover : MonoBehaviour
    {

        LerpVector3 lerper;

        public static void LerpMove(GameObject gObj, Vector3 destination, float seconds)
        {
            gObj.AddComponent<LerpMover>().lerper = new LerpVector3(gObj.transform.position, destination, seconds);
        }

        public void StopLerping()
        {
            enabled = false;
            Destroy(this);
        }

        private void Update()
        {
            if (!lerper.done())
            {
                transform.position = lerper.current();
            }
            else
            {
                Destroy(this);
            }
        }
    }
}