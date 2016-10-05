! mpif90 mtv2d_send_type_recv_int.f90; mpirun -n 1 ./a.out       
PROGRAM MTV

      USE MPI

      IMPLICIT NONE
      
      INTEGER ::   M=6, N=5
      INTEGER ::   S=3, T=2
      INTEGER, DIMENSION(:,:),  ALLOCATABLE :: A, B

      INTEGER :: I, J
      INTEGER :: TYPE_INT, SIZEOFINT, STATUS(MPI_STATUS_SIZE), IERR
      INTEGER :: RANK, NUMPROCS, DEST

      CALL MPI_INIT(IERR)
      CALL MPI_COMM_RANK(MPI_COMM_WORLD, RANK, IERR) 
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD, NUMPROCS, IERR) 

     ! Get size of MPI_INTEGER
     ! CALL MPI_TYPE_EXTENT(MPI_INTEGER, SIZEOFINT, IERR)

     ! count       T = 2 blocks (columns)
     ! blocklength S = 3 rows in each column
     ! Stride      M = 6 elements in one column in original matrix

      CALL MPI_TYPE_VECTOR(T, S, M, MPI_INTEGER, TYPE_INT, IERR)
      CALL MPI_TYPE_COMMIT(TYPE_INT, IERR)


      ALLOCATE  ( A (M,N) )
      ALLOCATE  ( B (S,T) )

      DO J=1,N
         DO I=1,M
            A(I,J)=I*10+J
         ENDDO
      ENDDO
      B = 99

      PRINT 1000, ((A(I,J), J=1,N), I=1,M)
1000  FORMAT(' A=', /, 6(5(1X,I4.0), /))

!--   Extract subset from 2nd to 4th row and 2nd to 4th column

!--   SENDRECV as MPI_INTEGER

!     CALL MPI_SENDRECV (A(2:S+1,2:T+1), S*T, MPI_INTEGER, 0, 111,   &
!                        B(1:S,1:T),     S*T, MPI_INTEGER, 0, 111, MPI_COMM_WORLD, STATUS, IERR)
 

!--   SEND as MPI_TYPE_VECTOR; RECV as MPI_INTEGER
!     Enough to specify the first element of A that needs to be sent.
!     Count of A is 1 of TYPE_INT

      CALL MPI_SENDRECV (A(2,2), 1, TYPE_INT, 0, 111, B(1:S,1:T), S*T, MPI_INTEGER, 0, 111, MPI_COMM_WORLD, STATUS, IERR)

!     CALL MPI_SENDRECV (A(2,2), 1, TYPE_INT, 0, 111, B,          S*T, MPI_INTEGER, 0, 111, MPI_COMM_WORLD, STATUS, IERR)

!--   WILL FAIL: RECV as DERIVED TYPE_INT. Unles MxN == SxT

!     CALL MPI_SENDRECV (A(2:S+1,2:T+1), S*T, MPI_INTEGER, 0, 111, B(1:S,1:T), S*T, MPI_INTEGER, 0, 111, MPI_COMM_WORLD, STATUS, IERR)

      PRINT 1001, ((B(I,J), J=1,T), I=1,S)
1001  FORMAT(' B=', /, 3(2(1X,I4.0), /))

      DEALLOCATE (A, B)
      CALL MPI_TYPE_FREE(TYPE_INT, IERR)
      CALL MPI_FINALIZE(IERR)

END
