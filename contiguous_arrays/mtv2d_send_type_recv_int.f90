! mpif90 mtv2d_recv_int.f90; mpirun -n 2 ./a.out       
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

      DEST = NUMPROCS-1

      IF ( RANK == 0) then

           ALLOCATE  ( A (M,N) )
           DO J=1,N
              DO I=1,M
                 A(I,J)=I*10+J
              ENDDO
           ENDDO

     PRINT 1000, ((A(I,J), J=1,N), I=1,M)
1000 FORMAT(' A=', /, 6(5(1X,I4.0), /))

!-- SEND AS MPI_INTEGER
         ! CALL MPI_SEND(A(2:S+1,2:T+1), S*T, MPI_INTEGER, DEST, 111, MPI_COMM_WORLD, STATUS, IERR)

 !-- SEND AS DERIVED TYPE_INT
           CALL MPI_SEND(A(2,2), 1, TYPE_INT, DEST, 111, MPI_COMM_WORLD, STATUS, IERR)
           DEALLOCATE (A)

      ELSE IF (RANK == DEST) THEN
	   print *, 'DEST'

           ALLOCATE  ( B (S,T) )
           B = 99

!-- RECV AS MPI_INTEGER
          CALL MPI_RECV(B(1:S,1:T), S*T, MPI_INTEGER, 0, 111, MPI_COMM_WORLD, STATUS, IERR)

!-- WILL FAIL: RECV AS DERIVED TYPE_INT. Unles MxN == SxT
       !  CALL MPI_RECV(B(1,1), 1, TYPE_INT, 0, 111, MPI_COMM_WORLD, STATUS, IERR)

     PRINT 1001, ((B(I,J), J=1,T), I=1,S)
1001 FORMAT(' B=', /, 3(2(1X,I4.0), /))

           DEALLOCATE (B)

      ENDIF

      CALL MPI_TYPE_FREE(TYPE_INT, IERR)
      CALL MPI_FINALIZE(IERR)

END
