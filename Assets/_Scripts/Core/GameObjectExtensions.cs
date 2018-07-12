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

        /// <summary>
        /// Lerps the global position to the target position over a number of seconds
        /// </summary>
        public static void MoveTo(this GameObject gObj, Vector3 destination, float seconds)
        {
            LerpMover.LerpMove(gObj, destination, seconds);
        }

        public static void LocalMoveTo(this GameObject gObj, Vector3 destination, float seconds)
        {
            LerpMover.LocalLerpMove(gObj, destination, seconds);
        }

        /// <summary>
        /// Lerps the global rotation to the target rotation over a number of seconds
        /// </summary>
        public static void RotateTo(this GameObject gObj, Quaternion destination, float seconds)
        {
            LerpRotator.LerpRotate(gObj, destination, seconds);
        }

        /// <summary>
        /// Lerps the local rotation to the target rotation over a number of seconds
        /// </summary>
        public static void LocalRotateTo(this GameObject gObj, Quaternion destination, float seconds)
        {
            LerpRotator.LocalLerpRotate(gObj, destination, seconds);
        }

        /// <summary>
        /// Lerps the localScale to the targetScale over a number of seconds
        /// </summary>
        public static void LocalScaleTo(this GameObject gObj, Vector3 destination, float seconds)
        {
            LerpScaler.LerpLocalScale(gObj, destination, seconds);
        }

        /// <summary>
        /// Lerps the lossyScale to the targetScale over a number of seconds
        /// </summary>
        public static void GlobalScaleTo(this GameObject gObj, Vector3 destination, float seconds)
        {
            LerpScaler.LerpGlobalScale(gObj, destination, seconds);
        }

        public static void SetGlobalScale(this Transform transform, Vector3 globalScale)
        {
            transform.localScale = Vector3.one;
            transform.localScale = new Vector3(globalScale.x / transform.lossyScale.x, globalScale.y / transform.lossyScale.y, globalScale.z / transform.lossyScale.z);
        }

        public static void EnsureNoneOf<T>(this GameObject gameObject) where T : Component
        {
            T[] c;
            c = gameObject.GetComponents<T>();
            foreach (T t in c)
            {
                gameObject.RemoveComponent<T>(t);
            }
        }

        public static void RemoveComponent<T>(this GameObject gameObject, T t) where T : Component
        {
#if UNITY_EDITOR
            if (!Application.isPlaying)
            {
                try
                {
                    UnityEngine.Object.DestroyImmediate(t, true);
                }
                catch (Exception e)
                {
                    Debug.Log("Failed to remove object of type " + t.ToString() + " from object " + gameObject);
                    throw (e);
                }

                return;
            }
#endif
            UnityEngine.Object.Destroy(t);
        }

        public static T EnsureOneOf<T>(this GameObject gameObject) where T : Component
        {
            T t = gameObject.GetComponent<T>();
            if (t == null)
            {
                t = gameObject.AddComponent<T>();
            }
            // else
            // {
            //     Debug.Log(gameObject.name + " already contains " + typeof(T));
            // }
            return t;
        }
    }

    public class LerpMover : Nanome.Core.Behaviour
    {
        LerpVector3 lerper;
        bool local;

        public static void LerpMove(GameObject gObj, Vector3 destination, float seconds)
        {
            gObj.EnsureNoneOf<LerpMover>();
            LerpMover l = gObj.AddComponent<LerpMover>();
            l.lerper = new LerpVector3(gObj.transform.position, destination, seconds);
            l.local = false;
        }

        public static void LocalLerpMove(GameObject gObj, Vector3 destination, float seconds)
        {
            gObj.EnsureNoneOf<LerpMover>();
            LerpMover l = gObj.AddComponent<LerpMover>();
            l.lerper = new LerpVector3(gObj.transform.localPosition, destination, seconds);
            l.local = true;
        }

        public void StopLerping()
        {
            enabled = false;
            Destroy(this);
        }

        private void Update()
        {
            if (lerper.done())
            {
                Destroy(this);
            }

            if (local)
            {
                transform.localPosition = lerper.current();
            }
            else
            {
                transform.position = lerper.current();
            }

        }
    }

    public class LerpRotator : Nanome.Core.Behaviour
    {
        LerpQuaternion lerper;

        bool local;


        public static void LerpRotate(GameObject gObj, Quaternion destination, float seconds)
        {
            Quaternion badQ = new Quaternion(0, 0, 0, 0);
            if (destination.Equals(badQ))
            {
                Debug.Log("Impossible lerpRotation being attempted on object " + gObj.name);
                return;
            }
            gObj.EnsureNoneOf<LerpRotator>();
            LerpRotator l = gObj.AddComponent<LerpRotator>();
            l.lerper = new LerpQuaternion(gObj.transform.localRotation, destination, seconds);
            l.local = true;
        }

        public static void LocalLerpRotate(GameObject gObj, Quaternion destination, float seconds)
        {
            Quaternion badQ = new Quaternion(0, 0, 0, 0);
            if (destination.Equals(badQ))
            {
                Debug.Log("Impossible lerpRotation being attempted on object " + gObj.name);
                return;
            }
            gObj.EnsureNoneOf<LerpRotator>();
            LerpRotator l = gObj.AddComponent<LerpRotator>();
            l.lerper = new LerpQuaternion(gObj.transform.localRotation, destination, seconds);
            l.local = true;
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
                if (local)
                {
                    transform.localRotation = lerper.current();

                }
                else
                {
                    transform.rotation = lerper.current();
                }
            }
            else
            {
                Destroy(this);
            }
        }
    }

    public class LerpScaler : Nanome.Core.Behaviour
    {
        LerpVector3 lerper;
        bool global = false;

        public static void LerpLocalScale(GameObject gObj, Vector3 destination, float seconds)
        {
            gObj.EnsureNoneOf<LerpScaler>();
            LerpScaler s = gObj.AddComponent<LerpScaler>();
            s.lerper = new LerpVector3(gObj.transform.localScale, destination, seconds);
            s.global = false;
        }

        public static void LerpGlobalScale(GameObject gObj, Vector3 destination, float seconds)
        {
            gObj.EnsureNoneOf<LerpScaler>();
            LerpScaler s = gObj.AddComponent<LerpScaler>();
            s.lerper = new LerpVector3(gObj.transform.lossyScale, destination, seconds);
            s.global = true;
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
                if (!global)
                {
                    transform.localScale = lerper.current();
                }
                else
                {
                    transform.SetGlobalScale(lerper.current());
                }
            }
            else
            {
                Destroy(this);
            }
        }
    }
}